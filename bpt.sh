#!/bin/bash
# vim: set foldlevel=0:
# shellcheck disable=SC2317

# Import once
if [[ -n $__DEFINED_BPT_SH ]]; then return; fi
readonly __DEFINED_BPT_SH=1

# Add multiple traps for EXIT (see: https://stackoverflow.com/questions/3338030)
bpt.__add_exit_trap() {
    trap_add_cmd=${1:?${FUNCNAME[0]} usage error}
    trap -- "$(
        extract_trap_cmd() { printf '%s\n' "$3"; }
        eval "extract_trap_cmd $(trap -p EXIT)"
        printf '%s\n' "${trap_add_cmd}"
    )" EXIT || echo "Unable to add to trap" >&2
}

# The shift-reduce LR(1) parser.
# $1: Parse table name.
#   The parse table should be an associative array where the key is
#   <state>,<token> and the value actions (s <k>/r <rule>/a/<k>).
# $2: Reduce function hook
#   This function can access `rule=(LHS RHS1 RHS2 ...)`.
#   This function can access the RHS(i+1)'s content': `${contents[$((s + i))]}`.
#   This function should store the reduce result to `contents[$s]`.
# $3: Error handler function hook
#   Args passed to this function:
#     $1: Line
#     $2: Column
#     $3: Default error message
# $4: (optional) If set, enable debug.
# shellcheck disable=SC2030 # Modification to `contents` and `rule` are local.
bpt.__lr_parse() (
    local -rn table="$1"
    local -r reduce_fn="${2:-echo}"
    local -r error_fn="${3:-__error}"
    if [[ -n $4 ]]; then local -r NDEBUG=false; else local -r NDEBUG=true; fi

    # 20 should be enough ...
    # I assume no one's writing a BNF with more than 20 RHSs ...
    local -a STATE_PTRN=('')
    for i in {1..20}; do STATE_PTRN[i]="${STATE_PTRN[i - 1]}:*"; done

    # Parse stack
    #   Using string manipulation for states is faster than using an array.
    local states=':0' stack_size=1
    # Contents stack associatied wit the parse stack
    #   Large dict indexing is significantly faster than a regular one.
    #   Thus we use stack_size + associative array to emulate a regular array.
    local -A contents=([0]='')

    # Current reduction rule
    local -a rule=()
    # Current look-ahead token and its content
    local token='' content='' action=''
    # Location tracking variables
    local num_lines=0 num_bytes=0
    # Temporary variables
    local i=0 str_lines=0 buffer=''

    # $1: Goto state after shift
    __shift() {
        states+=":$1"
        contents["$stack_size"]="$content"
        ((++stack_size))
        token='' content=''
    }

    # $1: Rule
    __reduce() {
        # Although not robust, word splitting is faster than `read`
        # shellcheck disable=SC2206
        local num_rhs=$((${#rule[@]} - 1))

        # Reduce and goto state
        # shellcheck disable=SC2295
        states="${states%${STATE_PTRN[$num_rhs]}}"
        states+=":${table["${states##*:},${rule[0]}"]}"

        # Reduction start location (on the contents stack)
        local s=$((stack_size - num_rhs))

        # Run reduce hook (which saves the reduce result to `contents[$s]`)
        $reduce_fn || exit 1
        stack_size=$((s + 1))
    }

    # Simply print the result
    __accept() {
        printf '%s' "${contents[1]}"
    }

    # Default error handler
    __error() {
        echo "Error: Line $(($1 + 1)) Column $(($2 + 1))"
        echo "$3"
    } >&2

    # Debugging support
    $NDEBUG || {
        eval __orig"$(declare -f __shift)"
        eval __orig"$(declare -f __reduce)"
        eval __orig"$(declare -f __accept)"
        __shift() {
            echo "[DBG] ${states##*:} Shift $1 \`$content\`" >&2
            __orig__shift "$@"
        }
        __reduce() {
            echo "[DBG] ${states##*:} Reduce ${rule[*]}" >&2
            __orig__reduce
        }
        __accept() {
            $NDEBUG || echo "[DBG] Result accepted" >&2
            __orig__accept
        }
    }

    while true; do
        [[ -n $token ]] || {
            read -r token str_lines num_lines num_bytes
            IFS= read -r buffer || return 1
            content="$buffer"
            for ((i = 1; i < str_lines; ++i)); do
                IFS= read -r buffer || return 1
                content+=$'\n'"$buffer"
            done
        }

        action="${table["${states##*:},$token"]}"
        case "$action" in
        # Shift
        s*) __shift "${action#s }" ;;
        # Reduce
        r*) # shellcheck disable=SC2206
            rule=(${action#r })
            __reduce
            ;;
        # Accept
        a) __accept && break ;;
        # Error
        '')
            local expects='' rule_key=''
            for rule_key in "${!table[@]}"; do
                [[ $rule_key != "${states##*:},"* ||
                    -z "${table["$rule_key"]}" ||
                    "${table["$rule_key"]}" =~ ^[[:digit:]]+$ ]] ||
                    expects+="${expects:+,}${BPT_PP_TOKEN_TABLE["${rule_key##*,}"]:-${rule_key##*,}}"
            done
            $error_fn "$num_lines" "$num_bytes" \
                "Expects one of \`${expects[*]}\` but got \`${token}\` ($content)."
            $NDEBUG || echo "[DBG] PARSER STATES ${states} TOKEN ${token} CONTENT ${content}." >&2
            exit 1
            ;;
        *) # Parse table error (internal error)
            echo "Internal error: STATES ${states} TOKEN ${token} CONTENT ${content}. " >&2
            echo "Internal error: action '$action' not recognized." >&2
            exit 1
            ;;
        esac
    done
)

# The tokenizer for bpt
# $1: Left deilmiter
# $2: Right delimiter
# $3: Error handler function hook
#   Args passed to this function:
#     $1: Line
#     $2: Column
#     $3: Default error message
#
# Terminal token name to content mappings:
#   str: Anything outside the toplevel `ld ... rd` or
#        Anything inside `"..."` or `'...'` within any `ld ... rd`
#     Note1: `"` inside `"..."` needs to be escaped using `\"`,
#            and the same for `'` inside `'...'`.
#
#   ld: ${ldelim}   rd: ${rdelim}       lp: (           rp: )
#   cl: :           ex: !               eq: -eq         ne: -ne
#   lt: -lt         gt: -gt             le: -le         ge: -ge
#   streq: ==       strne: !=           strlt: <        strgt: >
#   and|or|if|elif|else|for|in|include: <as is>
#   id: [[:alpha:]_][[:alnum:]_]*
bpt.scan() (
    local -r ld="$1" rd="$2" error_fn="${3:-__error}"
    bpt.__test_delims "$ld" "$rd" || return 1

    # Default error handler
    __error() {
        echo "Error: Line $(($1 + 1)) Column $(($2 + 1))"
        echo "$3"
    } >&2

    # See man regex.7. We need to escape the meta characters of POSIX regex.
    local -rA ESC=(
        ['^']=\\ ['.']=\\ ['[']=\\ ['$']=\\ ['(']=\\ [')']=\\
        ['|']=\\ ['*']=\\ ['+']=\\ ['?']=\\ ['{']=\\ [\\]=\\
    )
    local e_ld='' e_rd='' i=0
    for ((i = 0; i < ${#ld}; ++i)); do e_ld+="${ESC["${ld:i:1}"]}${ld:i:1}"; done
    for ((i = 0; i < ${#rd}; ++i)); do e_rd+="${ESC["${rd:i:1}"]}${rd:i:1}"; done

    # Keywords
    local -ra KW=(
        "${e_ld}" "${e_rd}"
        '-eq' '-ne' '-gt' '-lt' '-ge' '-le'
        '==' '!=' '>' '<' ':' '\!' '"' "'" '\(' '\)'
        'and' 'or' 'if' 'elif' 'else' 'for' 'in' 'include'
    )
    local -r KW_RE="$(IFS='|' && echo -n "${KW[*]}")"
    local -r ID_RE='[[:alpha:]_][[:alnum:]_]*'

    # Scanner states
    local num_ld=0
    local quote=''

    # Location trackers
    local num_lines=0
    local num_bytes=0

    # String processing tracker & buffer.
    # `str_lines=''` means currently outside the scope of string
    local string='' str_lines='' str_bytes=''
    # Start scannign string
    __start_string() {
        str_lines="${str_lines:-1}"
        str_bytes="${str_bytes:-$num_bytes}"
    }
    # Commit (possibly multiline) string buffer
    # shellcheck disable=SC2031
    __commit_string() {
        ((str_lines > 0)) || return
        echo "str $str_lines $((num_lines + 1 - str_lines)) $str_bytes"
        # `$content` can be a literal `-ne`. Thus printf is needed.
        printf '%s\n' "$string"
        string='' str_lines='' str_bytes=''
    }

    # Tokenizer
    local line='' content=''
    while IFS= read -r line; do
        # Decide whether currently scanning a string
        # Only count newlines in strings (outside `ld ... rd` and inside quotes).
        [[ $num_ld -gt 0 && -z "$quote" ]] || {
            __start_string
            [[ $num_lines -eq 0 ]] || { string+=$'\n' && ((++str_lines)); }
        }

        # Scan the line
        while [[ -n "$line" ]]; do
            content='' # The consumed content (to be removed from `line`)
            if [[ $num_ld -eq 0 ]]; then
                # Outside `ld ... rd`
                if [[ $line =~ ^(${e_ld}) ]]; then
                    # If met `ld`, enter `ld ... rd`
                    __commit_string
                    ((++num_ld))
                    content="${BASH_REMATCH[1]}"
                    echo "ld 1 $num_lines $num_bytes"
                    printf '%s\n' "$content"
                elif [[ $line =~ (${e_ld}) ]]; then
                    content="${line%%"${BASH_REMATCH[1]}"*}"
                    string+="$content"
                else
                    content="$line"
                    string+="$line"
                fi
            elif [[ -n "$quote" ]]; then
                # Inside quotes in `ld ... rd`
                # Scan for `str` until we find a non-escaped quote.
                local line_copy="$line"
                while [[ $line_copy =~ ^[^${quote}]*\\${quote} ]]; do
                    # Escape quote inside string
                    string+="${line_copy%%"\\${quote}"*}${quote}"
                    content+="${line_copy%%"\\${quote}"*}\\${quote}"
                    line_copy="${line_copy#"${BASH_REMATCH[0]}"}"
                done

                if [[ $line_copy =~ ${quote} ]]; then
                    # Remove the closing quote from line
                    content+="${line_copy%%"${quote}"*}${quote}"
                    string+="${line_copy%%"${quote}"*}"
                    quote=''
                    __commit_string
                else
                    content="$line_copy"
                    string+="$line_copy"
                fi
            else
                # Non-strings. Commit string first.
                __commit_string
                if [[ $line =~ ^(${KW_RE}) ]]; then
                    # Inside `ld ... rd` and matches a keyword at front
                    content="${BASH_REMATCH[1]}"
                    case "$content" in
                    '-eq') echo -n eq ;; '-ne') echo -n ne ;;
                    '-lt') echo -n lt ;; '-gt') echo -n gt ;;
                    '-le') echo -n le ;; '-ge') echo -n ge ;;
                    '==') echo -n streq ;; '!=') echo -n strne ;;
                    '>') echo -n strgt ;; '<') echo -n strlt ;;
                    '!') echo -n ex ;; ':') echo -n cl ;;
                    '(') echo -n lp ;; ')') echo -n rp ;;
                    '"' | "'")
                        quote="$content"
                        __start_string
                        ;;
                    "$ld")
                        ((++num_ld))
                        echo -n ld
                        ;;
                    "$rd")
                        ((num_ld-- > 0)) || {
                            $error_fn "$num_lines" "$num_bytes" "Extra '$rd'."
                            return 1
                        }
                        ((num_ld != 0)) || __start_string
                        echo -n rd
                        ;;
                    and | or | if | elif | else) ;&
                    for | in | include) echo -n "$content" ;;
                    *)
                        $error_fn "$num_lines" "$num_bytes" \
                            "Internal error: Unrecognized token ${content}"
                        return 1
                        ;;
                    esac
                    [[ -n $quote ]] || {
                        echo " 1 $num_lines $num_bytes"
                        printf '%s\n' "$content"
                    }
                else # Inside `ld ... rd` but outside quotes
                    # Ignore spaces inside `ld ... rd`
                    [[ $line =~ ^([[:space:]]+)(.*) ]] && {
                        line="${BASH_REMATCH[2]}"
                        ((num_bytes += ${#BASH_REMATCH[1]}))
                        continue
                    }
                    content="$line"

                    # Contents are either keywords or identifiers
                    if [[ $content =~ (${KW_RE}) ]]; then
                        content="${content%%"${BASH_REMATCH[1]}"*}"
                    fi
                    if [[ ! $content =~ ^(${ID_RE}) ]]; then
                        $error_fn "$num_lines" "$num_bytes" \
                            "'$content' is not a valid identifier"
                        return 1
                    fi
                    content="${BASH_REMATCH[1]}"
                    echo "id 1 $num_lines $num_bytes"
                    printf '%s\n' "$content"
                fi
            fi

            # Post-processing only counts the last line read.
            line="${line#"$content"}"
            ((num_bytes += ${#content}))
        done
        ((++num_lines))
        num_bytes=0 content=''
    done
    __commit_string
    echo "$ 1 $num_lines 0" # The EOF token
    echo ''                 # The EOF content (empty)
)

bpt.__test_delims() {
    [[ $1 != *' '* && $2 != *' '* ]] || {
        echo "Left and right delimiters must not contain spaces." >&2
        return 1
    }
    [[ "$1" != "$2" ]] || {
        echo "Left and right delimiters must be different." >&2
        return 1
    }
}

# The reduce function to collect all variables
# shellcheck disable=SC2031 # Direct access of `rule` and `contents` for speed.
bpt.__reduce_collect_vars() {
    case "${rule[0]}" in
    UOP | BOP) contents[$s]='' ;;
    VAR)
        contents[$s]="${contents[$((s + 1))]}"$'\n'
        [[ ${#rule[@]} -eq 4 || ${rule[4]} != VAR ]] || contents[$s]+="${contents[$((s + 3))]}"
        ;;
    BUILTIN) contents[$s]="${contents[$((s + 3))]}" ;;
    INCLUDE) contents[$s]="$(__recursive_process "${contents[$((s + 3))]}")"$'\n' ;;
    FORIN)
        contents[$s]=''
        local var
        while read -r var; do
            [[ -z $var || $var == "${contents[$((s + 2))]}" ]] || contents[$s]+="$var"$'\n'
        done <<<"${contents[$((s + 6))]}"
        ;;
    BOOL) [[ "${#rule[@]}" -eq 2 ]] || contents[$s]+="${contents[$((s + 2))]}" ;;
    BOOLA)
        case "${#rule[@]}" in
        4) case "${rule[1]}" in
            lp) contents[$s]="${contents[$((s + 1))]}" ;;
            BOOLA) contents[$s]+="${contents[$((s + 2))]}" ;;
            esac ;;
        6) contents[$s]+="${contents[$((s + 3))]}" ;;
        esac
        ;;
    BOOLO) [[ "${#rule[@]}" -eq 2 ]] || contents[$s]+="${contents[$((s + 2))]}" ;;
    BOOLS) [[ "${#rule[@]}" -eq 2 ]] || contents[$s]="${contents[$((s + 1))]}" ;;
    ELSE)
        [[ "${#rule[@]}" -ne 1 ]] || { contents[$s]='' && return; }
        contents[$s]="${contents[$((s + 2))]}"
        ;;
    ELIF)
        [[ "${#rule[@]}" -ne 1 ]] || { contents[$s]='' && return; }
        contents[$s]="${contents[$((s + 2))]}${contents[$((s + 4))]}"
        ;;
    IF) contents[$s]="${contents[$((s + 2))]}${contents[$((s + 4))]}${contents[$((s + 5))]}${contents[$((s + 6))]}" ;;
    STMT) [[ "${rule[1]}" != STR ]] || contents[$s]='' ;;
    *)
        [[ "${#rule[@]}" -ne 1 ]] || { contents[$s]='' && return; }
        local i=1
        for (( ; i < ${#rule[@]} - 1; ++i)); do
            contents[$s]+="${contents[$((s + i))]}"
        done
        ;;
    esac
}

# The reduce function to collect all includes
# shellcheck disable=SC2031
bpt.__reduce_collect_includes() {
    case "${rule[0]}" in
    INCLUDE)
        contents[$s]="${contents[$((s + 3))]}"$'\n'
        contents[$s]+="$(__recursive_process "${contents[$((s + 3))]}")"
        ;;
    STMT) [[ "${rule[1]}" == INCLUDE ]] || contents[$s]='' ;;
    *)
        [[ "${#rule[@]}" -ne 1 ]] || { contents[$s]='' && return; }
        local i=1
        for (( ; i < ${#rule[@]} - 1; ++i)); do
            contents[$s]+="${contents[$((s + i))]}"
        done
        ;;
    esac
}

# The reduce function to generate the template
# shellcheck disable=SC2031
bpt.__reduce_generate() {
    case "${rule[0]}" in
    # Note: Since `contents[$s]` is exactly the first RHS, the
    #   `${contents[$s]}="${contents[$s]}"` assignment is unnecessary here.
    STR | ID | UOP | BOP) ;;
    VAR)
        case "${rule[3]}" in
        rd) contents[$s]="\${${contents[$((s + 1))]}}" ;;
        or) contents[$s]="\${${contents[$((s + 1))]}:-\$(e " ;;&
        and) contents[$s]="\${${contents[$((s + 1))]}:+\$(e " ;;&
        *) case "${rule[4]}" in
            VAR) contents[$s]+="\"${contents[$((s + 3))]}\")}" ;;
            STR) contents[$s]+="${contents[$((s + 3))]@Q})}" ;;
            esac ;;
        esac
        ;;
    ARGS)
        [[ "${#rule[@]}" -ne 1 ]] || { contents[$s]='' && return; }
        # Strip the tag from STMT
        local stmt_type="${contents[$((s + 1))]%%:*}"
        local stmt="${contents[$((s + 1))]#*:}"

        # Note: `${stmt@Q}` is faster than `printf '%q' ${stmt}`
        case "$stmt_type" in
        STR) contents[$s]+=" ${stmt@Q} " ;;
        VAR | BUILTIN) contents[$s]+=" $stmt " ;;
        INCLUDE | FORIN | IF) contents[$s]+=" \"\$($stmt)\" " ;;
        esac
        ;;
    BUILTIN)
        # Filter allowed builtints
        case "${contents[$((s + 1))]}" in
        len | seq) contents[$s]="\$(${contents[$((s + 1))]} ${contents[$((s + 3))]})" ;;
        quote) contents[$s]="\"\$(e ${contents[$((s + 3))]})\"" ;;
        *) echo "Unrecognized builtin function ${contents[$((s + 1))]}" >&2 && exit 1 ;;
        esac
        ;;
    INCLUDE) contents[$s]="$(__recursive_process "${contents[$((s + 3))]}")" ;;
    FORIN) contents[$s]="for ${contents[$((s + 2))]} in ${contents[$((s + 4))]}; do ${contents[$((s + 6))]} done" ;;
    BOOL)
        case "${#rule[@]}" in
        2) contents[$s]="\"\$(e ${contents[$s]})\"" ;;
        4) contents[$s]+=" ${contents[$((s + 1))]} \"\$(e ${contents[$((s + 2))]})\"" ;;
        esac
        ;;
    BOOLA)
        case "${#rule[@]}" in
        4) case "${rule[1]}" in
            BOOLA) contents[$s]="${contents[$s]} && ${contents[$((s + 2))]}" ;;
            lp) contents[$s]="( ${contents[$((s + 1))]} )" ;;
            esac ;;
        6) contents[$s]="${contents[$s]} && ( ${contents[$((s + 3))]} )" ;;
        esac
        ;;
    BOOLO) [[ ${#rule[@]} -eq 2 ]] || contents[$s]="${contents[$s]} || ${contents[$((s + 2))]}" ;;
    BOOLS) [[ ${#rule[@]} -eq 2 ]] || contents[$s]="${contents[$s]} ${contents[$((s + 1))]}" ;;
    ELSE)
        case "${#rule[@]}" in
        1) contents[$s]='' ;;
        *) contents[$s]="else ${contents[$((s + 2))]}" ;;
        esac
        ;;
    ELIF)
        case "${#rule[@]}" in
        1) contents[$s]='' ;;
        *) contents[$s]="${contents[$s]} elif [[ ${contents[$((s + 2))]} ]]; then ${contents[$((s + 4))]}" ;;
        esac
        ;;
    IF) contents[$s]="if [[ ${contents[$((s + 2))]} ]]; then ${contents[$((s + 4))]}${contents[$((s + 5))]}${contents[$((s + 6))]} fi" ;;
    STMT)
        # Tag the sub-type to the reduce result
        # (Need to strip the tag wherever STMT is used)
        contents[$s]="${rule[1]}:${contents[$s]}"
        ;;
    DOC) # Similar to ARGS but produces commands instead of strings
        # Return when document is empty
        [[ "${#rule[@]}" -ne 1 ]] || { contents[$s]='' && return; }

        # Strip the tag from STMT
        local stmt_type="${contents[$((s + 1))]%%:*}"
        local stmt="${contents[$((s + 1))]#*:}"

        # Reduce the document
        case "$stmt_type" in
        STR) contents[$s]+="{ e ${stmt@Q}; };" ;;
        BUILTIN | VAR) contents[$s]+="{ e \"$stmt\"; };" ;;
        INCLUDE) contents[$s]+="$stmt" ;;
        FORIN | IF) contents[$s]+="{ $stmt; };" ;;
        esac
        ;;
    *) echo "Internal error: Rule ${rule[*]} not recognized" >&2 ;;
    esac
}

# Process the template
#
# $1: Left deilmiter
# $2: Right delimiter
# $3: The reduce function hook to pass to the parser.
#     Defaults to bpt.__reduce_collect_vars
# $4: File to process
# $5: (optional) If set, enable debug.
#
# Input: Template from stdin
#
# Grammar:
#   DOC     -> DOC STMT
#            | .
#   STMT    -> IF | FORIN | INCLUDE | BUILTIN | VAR | STR .
#   IF      -> ld if BOOLS cl DOC ELIF ELSE rd .
#   ELIF    -> ELIF elif BOOLS cl DOC
#            | .
#   ELSE    -> else cl DOC
#            | .
#   BOOLS   -> BOOLO
#            | UOP BOOLS .
#   BOOLO   -> BOOLO or BOOLA
#            | BOOLA .
#   BOOLA   -> BOOLA and BOOL
#            | BOOLA and lp BOOLS rp
#            | lp BOOLS rp
#            | BOOL .
#   BOOL    -> ARGS BOP ARGS
#            | ARGS .
#   FORIN   -> ld for ID in ARGS cl DOC rd .
#   INCLUDE -> ld include cl STR rd .
#   BUILTIN -> ld ID cl ARGS rd .
#   ARGS    -> ARGS STMT
#            | .
#   VAR     -> ld ID rd
#            | ld ID or VAR rd
#            | ld ID or STR rd
#            | ld ID and VAR rd
#            | ld ID and STR rd .
#   BOP     -> ne | eq | gt | lt | ge | le | strgt | strlt | streq | strne .
#   UOP     -> ex .
#   ID      -> id .
#   STR     -> str .
#
# Note1: the combination of BOOLO and BOOLA is equivalent to the expression
#   grammar `BOOLS -> BOOL or BOOL | BOOL and BOOL | lp BOOL rp | BOOL .`
#   with left associativity for `and` and `or` meanwhile `and` having higher
#   precidence than `or`. (Solves shift-reduce conflict with subclassing).
# See: https://ece.uwaterloo.ca/~vganesh/TEACHING/W2014/lectures/lecture08.pdf
#
# Note2: the `ID -> id` and `STR -> str` rules are not redundant.
#   They are for better controls when hooking the reduce function.
bpt.process() (
    local -r ld="$1" rd="$2" file="$4" debug="$5"
    local -r reduce_fn="${3:-bpt.__reduce_generate}"
    local -a file_stack=("${file_stack[@]}")

    [[ -f $file ]] || {
        echo "Error: file '$file' does not exist" >&2
        return 1
    }
    file_stack+=("$file")

    # Curry this function so that it can be called by the reducer recursively
    __recursive_process() {
        local file
        # Detect recursive includes
        for file in "${file_stack[@]}"; do
            [[ $file -ef $1 ]] && {
                printf "Error: recursive include detected:\n"
                printf '  In: %s\n' "${file_stack[0]}"
                printf '  --> %s\n' "${file_stack[@]:1}"
                printf '  --> %s\n' "${file}"
                return 1
            } >&2
        done
        bpt.process "$ld" "$rd" "$reduce_fn" "$1" "$debug"
    }

    # Pretty-print parse errors
    __error_handler() {
        echo "Error: File '$file' Line $(($1 + 1)) Column $(($2 + 1))"
        echo "$3"
        echo
        # Pretty-print the error location
        local -a line=()
        mapfile -t -s "$1" -n 1 line <"$file"
        echo "${line[0]}"
        printf "%$2s^--- \033[1mHERE\033[0m\n"
    } >&2

    # Prase with the provided reduce function
    bpt.__lr_parse BPT_PARSE_TABLE "$reduce_fn" __error_handler "$debug" \
        < <(bpt.scan "$ld" "$rd" __error_handler <"$file")
)

# $1: Left deilmiter
# $2: Right delimiter
# $3: File to process
# $4: (optional) If set, enable debug.
# shellcheck disable=SC2207
bpt.fingerprint() {
    local -r ld="$1" rd="$2" file="$3" debug="$4"

    # Collect vars and includes
    local -a vars=() incs=()
    mapfile -t vars < <(bpt.__dedup "$(bpt.process "$ld" "$rd" bpt.__reduce_collect_vars "$infile" "$debug")")
    mapfile -t incs < <(bpt.__dedup "$(bpt.process "$ld" "$rd" bpt.__reduce_collect_includes "$infile" "$debug")")
    local fingerprint=''
    local -a md5=()

    # Hash this script (the generator)
    md5=($(md5sum "${BASH_SOURCE[0]}")) && fingerprint+="M:${md5[0]}"

    # Hash the file itself
    md5=($(md5sum "$file")) && fingerprint+=":S:${md5[0]}"

    # Digest the includes
    for inc in "${incs[@]}"; do
        md5=($(md5sum "$inc")) && fingerprint+=":I:${md5[0]}"
    done
    # Digest and check for missing vars
    for var in "${vars[@]}"; do
        md5=($(echo -n "${var}${!var}" | md5sum)) && fingerprint+=":V:${md5[0]}"
    done

    # Digest the digests
    [[ $debug ]] && echo "[DBG] Raw fingerprint: $fingerprint"
    md5=($(echo -n "${fingerprint}" | md5sum)) && fingerprint="${md5[0]}"
    echo "$fingerprint"
}

bpt.print_help() {
    :
}

bpt.main() {
    local ld='{{' rd='}}'
    local infile=''
    local cmd='' reduce_fn=bpt.__reduce_generate post_process=eval
    local debug=''

    # Parse command
    case "$1" in
    scan | s) cmd=scan ;;
    generate | g)
        cmd=generate
        reduce_fn=bpt.__reduce_generate
        post_process='echo'
        ;;
    generate-eval | ge)
        cmd=generate-eval
        reduce_fn=bpt.__reduce_generate
        post_process='eval'
        ;;
    collect-vars | cv)
        cmd=collect-vars
        reduce_fn=bpt.__reduce_collect_vars
        post_process=bpt.__dedup
        ;;
    collect-includes | ci)
        cmd=collect-includes
        reduce_fn=bpt.__reduce_collect_includes
        post_process=bpt.__dedup
        ;;
    fingerprint | f)
        cmd=fingerprint
        ;;
    *)
        echo "Unrecognized command '$1'" >&2
        bpt.print_help
        exit 1
        ;;
    esac
    shift

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
        -l | --left-delimiter) shift && ld="$1" ;;
        -r | --right-delimiter) shift && rd="$1" ;;
        -d | --debug) debug=1 ;;
        *)
            [[ -z $infile ]] || {
                echo "Error: Option '$1' not recognized." >&2
                bpt.print_help
                return 1
            }
            infile="$1"
            ;;
        esac
        shift
    done

    # If file not provided, read into a temporary file and use that file.
    [[ -n $infile ]] || {
        infile="$(mktemp)" || { echo "Error: mktemp failed." >&2 && exit 1; }
        bpt.__add_exit_trap "rm -f \"${infile:?}\""
        cat >"${infile:?}"
    }

    # Global constants for pretty-printing
    local -rA BPT_PP_TOKEN_TABLE=(
        [ld]="$ld" [rd]="$rd"
        [eq]='-eq' [ne]='-ne' [gt]='-gt' [lt]='-lt' [ge]='-ge' [le]='-le'
        [streq]='==' [strne]='!=' [strgt]='>' [strlt]='<'
        [cl]=':' [ex]='!' [lp]='(' [rp]=')')

    # shellcheck disable=SC2034
    # >>> BPT_PARSE_TABLE_S >>>
    local -rA BPT_PARSE_TABLE=( # {{{
        ["0,ld"]="r DOC" ["0,str"]="r DOC" ["0,$"]="r DOC" ["0,DOC"]="1" ["1,ld"]="s 9"
        ["1,str"]="s 10" ["1,$"]="a" ["1,STMT"]="2" ["1,IF"]="3" ["1,FORIN"]="4"
        ["1,INCLUDE"]="5" ["1,BUILTIN"]="6" ["1,VAR"]="7" ["1,STR"]="8"
        ["2,ld"]="r DOC DOC STMT" ["2,rd"]="r DOC DOC STMT" ["2,elif"]="r DOC DOC STMT"
        ["2,else"]="r DOC DOC STMT" ["2,str"]="r DOC DOC STMT" ["2,$"]="r DOC DOC STMT"
        ["3,ld"]="r STMT IF" ["3,cl"]="r STMT IF" ["3,rd"]="r STMT IF"
        ["3,elif"]="r STMT IF" ["3,else"]="r STMT IF" ["3,or"]="r STMT IF"
        ["3,and"]="r STMT IF" ["3,rp"]="r STMT IF" ["3,ne"]="r STMT IF"
        ["3,eq"]="r STMT IF" ["3,gt"]="r STMT IF" ["3,lt"]="r STMT IF"
        ["3,ge"]="r STMT IF" ["3,le"]="r STMT IF" ["3,strgt"]="r STMT IF"
        ["3,strlt"]="r STMT IF" ["3,streq"]="r STMT IF" ["3,strne"]="r STMT IF"
        ["3,str"]="r STMT IF" ["3,$"]="r STMT IF" ["4,ld"]="r STMT FORIN"
        ["4,cl"]="r STMT FORIN" ["4,rd"]="r STMT FORIN" ["4,elif"]="r STMT FORIN"
        ["4,else"]="r STMT FORIN" ["4,or"]="r STMT FORIN" ["4,and"]="r STMT FORIN"
        ["4,rp"]="r STMT FORIN" ["4,ne"]="r STMT FORIN" ["4,eq"]="r STMT FORIN"
        ["4,gt"]="r STMT FORIN" ["4,lt"]="r STMT FORIN" ["4,ge"]="r STMT FORIN"
        ["4,le"]="r STMT FORIN" ["4,strgt"]="r STMT FORIN" ["4,strlt"]="r STMT FORIN"
        ["4,streq"]="r STMT FORIN" ["4,strne"]="r STMT FORIN" ["4,str"]="r STMT FORIN"
        ["4,$"]="r STMT FORIN" ["5,ld"]="r STMT INCLUDE" ["5,cl"]="r STMT INCLUDE"
        ["5,rd"]="r STMT INCLUDE" ["5,elif"]="r STMT INCLUDE"
        ["5,else"]="r STMT INCLUDE" ["5,or"]="r STMT INCLUDE" ["5,and"]="r STMT INCLUDE"
        ["5,rp"]="r STMT INCLUDE" ["5,ne"]="r STMT INCLUDE" ["5,eq"]="r STMT INCLUDE"
        ["5,gt"]="r STMT INCLUDE" ["5,lt"]="r STMT INCLUDE" ["5,ge"]="r STMT INCLUDE"
        ["5,le"]="r STMT INCLUDE" ["5,strgt"]="r STMT INCLUDE"
        ["5,strlt"]="r STMT INCLUDE" ["5,streq"]="r STMT INCLUDE"
        ["5,strne"]="r STMT INCLUDE" ["5,str"]="r STMT INCLUDE" ["5,$"]="r STMT INCLUDE"
        ["6,ld"]="r STMT BUILTIN" ["6,cl"]="r STMT BUILTIN" ["6,rd"]="r STMT BUILTIN"
        ["6,elif"]="r STMT BUILTIN" ["6,else"]="r STMT BUILTIN"
        ["6,or"]="r STMT BUILTIN" ["6,and"]="r STMT BUILTIN" ["6,rp"]="r STMT BUILTIN"
        ["6,ne"]="r STMT BUILTIN" ["6,eq"]="r STMT BUILTIN" ["6,gt"]="r STMT BUILTIN"
        ["6,lt"]="r STMT BUILTIN" ["6,ge"]="r STMT BUILTIN" ["6,le"]="r STMT BUILTIN"
        ["6,strgt"]="r STMT BUILTIN" ["6,strlt"]="r STMT BUILTIN"
        ["6,streq"]="r STMT BUILTIN" ["6,strne"]="r STMT BUILTIN"
        ["6,str"]="r STMT BUILTIN" ["6,$"]="r STMT BUILTIN" ["7,ld"]="r STMT VAR"
        ["7,cl"]="r STMT VAR" ["7,rd"]="r STMT VAR" ["7,elif"]="r STMT VAR"
        ["7,else"]="r STMT VAR" ["7,or"]="r STMT VAR" ["7,and"]="r STMT VAR"
        ["7,rp"]="r STMT VAR" ["7,ne"]="r STMT VAR" ["7,eq"]="r STMT VAR"
        ["7,gt"]="r STMT VAR" ["7,lt"]="r STMT VAR" ["7,ge"]="r STMT VAR"
        ["7,le"]="r STMT VAR" ["7,strgt"]="r STMT VAR" ["7,strlt"]="r STMT VAR"
        ["7,streq"]="r STMT VAR" ["7,strne"]="r STMT VAR" ["7,str"]="r STMT VAR"
        ["7,$"]="r STMT VAR" ["8,ld"]="r STMT STR" ["8,cl"]="r STMT STR"
        ["8,rd"]="r STMT STR" ["8,elif"]="r STMT STR" ["8,else"]="r STMT STR"
        ["8,or"]="r STMT STR" ["8,and"]="r STMT STR" ["8,rp"]="r STMT STR"
        ["8,ne"]="r STMT STR" ["8,eq"]="r STMT STR" ["8,gt"]="r STMT STR"
        ["8,lt"]="r STMT STR" ["8,ge"]="r STMT STR" ["8,le"]="r STMT STR"
        ["8,strgt"]="r STMT STR" ["8,strlt"]="r STMT STR" ["8,streq"]="r STMT STR"
        ["8,strne"]="r STMT STR" ["8,str"]="r STMT STR" ["8,$"]="r STMT STR"
        ["9,if"]="s 11" ["9,for"]="s 12" ["9,include"]="s 13" ["9,id"]="s 15"
        ["9,ID"]="14" ["10,ld"]="r STR str" ["10,cl"]="r STR str" ["10,rd"]="r STR str"
        ["10,elif"]="r STR str" ["10,else"]="r STR str" ["10,or"]="r STR str"
        ["10,and"]="r STR str" ["10,rp"]="r STR str" ["10,ne"]="r STR str"
        ["10,eq"]="r STR str" ["10,gt"]="r STR str" ["10,lt"]="r STR str"
        ["10,ge"]="r STR str" ["10,le"]="r STR str" ["10,strgt"]="r STR str"
        ["10,strlt"]="r STR str" ["10,streq"]="r STR str" ["10,strne"]="r STR str"
        ["10,str"]="r STR str" ["10,$"]="r STR str" ["11,ld"]="r ARGS"
        ["11,cl"]="r ARGS" ["11,or"]="r ARGS" ["11,and"]="r ARGS" ["11,lp"]="s 21"
        ["11,ne"]="r ARGS" ["11,eq"]="r ARGS" ["11,gt"]="r ARGS" ["11,lt"]="r ARGS"
        ["11,ge"]="r ARGS" ["11,le"]="r ARGS" ["11,strgt"]="r ARGS"
        ["11,strlt"]="r ARGS" ["11,streq"]="r ARGS" ["11,strne"]="r ARGS"
        ["11,ex"]="s 20" ["11,str"]="r ARGS" ["11,BOOLS"]="16" ["11,BOOLO"]="17"
        ["11,UOP"]="18" ["11,BOOLA"]="19" ["11,BOOL"]="22" ["11,ARGS"]="23"
        ["12,id"]="s 15" ["12,ID"]="24" ["13,cl"]="s 25" ["14,cl"]="s 26"
        ["14,rd"]="s 27" ["14,or"]="s 28" ["14,and"]="s 29" ["15,cl"]="r ID id"
        ["15,rd"]="r ID id" ["15,or"]="r ID id" ["15,and"]="r ID id" ["15,in"]="r ID id"
        ["16,cl"]="s 30" ["17,cl"]="r BOOLS BOOLO" ["17,or"]="s 31"
        ["17,rp"]="r BOOLS BOOLO" ["18,ld"]="r ARGS" ["18,cl"]="r ARGS"
        ["18,or"]="r ARGS" ["18,and"]="r ARGS" ["18,lp"]="s 21" ["18,rp"]="r ARGS"
        ["18,ne"]="r ARGS" ["18,eq"]="r ARGS" ["18,gt"]="r ARGS" ["18,lt"]="r ARGS"
        ["18,ge"]="r ARGS" ["18,le"]="r ARGS" ["18,strgt"]="r ARGS"
        ["18,strlt"]="r ARGS" ["18,streq"]="r ARGS" ["18,strne"]="r ARGS"
        ["18,ex"]="s 20" ["18,str"]="r ARGS" ["18,BOOLS"]="32" ["18,BOOLO"]="17"
        ["18,UOP"]="18" ["18,BOOLA"]="19" ["18,BOOL"]="22" ["18,ARGS"]="23"
        ["19,cl"]="r BOOLO BOOLA" ["19,or"]="r BOOLO BOOLA" ["19,and"]="s 33"
        ["19,rp"]="r BOOLO BOOLA" ["20,ld"]="r UOP ex" ["20,cl"]="r UOP ex"
        ["20,or"]="r UOP ex" ["20,and"]="r UOP ex" ["20,lp"]="r UOP ex"
        ["20,rp"]="r UOP ex" ["20,ne"]="r UOP ex" ["20,eq"]="r UOP ex"
        ["20,gt"]="r UOP ex" ["20,lt"]="r UOP ex" ["20,ge"]="r UOP ex"
        ["20,le"]="r UOP ex" ["20,strgt"]="r UOP ex" ["20,strlt"]="r UOP ex"
        ["20,streq"]="r UOP ex" ["20,strne"]="r UOP ex" ["20,ex"]="r UOP ex"
        ["20,str"]="r UOP ex" ["21,ld"]="r ARGS" ["21,or"]="r ARGS" ["21,and"]="r ARGS"
        ["21,lp"]="s 21" ["21,rp"]="r ARGS" ["21,ne"]="r ARGS" ["21,eq"]="r ARGS"
        ["21,gt"]="r ARGS" ["21,lt"]="r ARGS" ["21,ge"]="r ARGS" ["21,le"]="r ARGS"
        ["21,strgt"]="r ARGS" ["21,strlt"]="r ARGS" ["21,streq"]="r ARGS"
        ["21,strne"]="r ARGS" ["21,ex"]="s 20" ["21,str"]="r ARGS" ["21,BOOLS"]="34"
        ["21,BOOLO"]="17" ["21,UOP"]="18" ["21,BOOLA"]="19" ["21,BOOL"]="22"
        ["21,ARGS"]="23" ["22,cl"]="r BOOLA BOOL" ["22,or"]="r BOOLA BOOL"
        ["22,and"]="r BOOLA BOOL" ["22,rp"]="r BOOLA BOOL" ["23,ld"]="s 9"
        ["23,cl"]="r BOOL ARGS" ["23,or"]="r BOOL ARGS" ["23,and"]="r BOOL ARGS"
        ["23,rp"]="r BOOL ARGS" ["23,ne"]="s 37" ["23,eq"]="s 38" ["23,gt"]="s 39"
        ["23,lt"]="s 40" ["23,ge"]="s 41" ["23,le"]="s 42" ["23,strgt"]="s 43"
        ["23,strlt"]="s 44" ["23,streq"]="s 45" ["23,strne"]="s 46" ["23,str"]="s 10"
        ["23,STMT"]="36" ["23,IF"]="3" ["23,FORIN"]="4" ["23,INCLUDE"]="5"
        ["23,BUILTIN"]="6" ["23,VAR"]="7" ["23,STR"]="8" ["23,BOP"]="35"
        ["24,in"]="s 47" ["25,str"]="s 10" ["25,STR"]="48" ["26,ld"]="r ARGS"
        ["26,rd"]="r ARGS" ["26,str"]="r ARGS" ["26,ARGS"]="49"
        ["27,ld"]="r VAR ld ID rd" ["27,cl"]="r VAR ld ID rd" ["27,rd"]="r VAR ld ID rd"
        ["27,elif"]="r VAR ld ID rd" ["27,else"]="r VAR ld ID rd"
        ["27,or"]="r VAR ld ID rd" ["27,and"]="r VAR ld ID rd"
        ["27,rp"]="r VAR ld ID rd" ["27,ne"]="r VAR ld ID rd" ["27,eq"]="r VAR ld ID rd"
        ["27,gt"]="r VAR ld ID rd" ["27,lt"]="r VAR ld ID rd" ["27,ge"]="r VAR ld ID rd"
        ["27,le"]="r VAR ld ID rd" ["27,strgt"]="r VAR ld ID rd"
        ["27,strlt"]="r VAR ld ID rd" ["27,streq"]="r VAR ld ID rd"
        ["27,strne"]="r VAR ld ID rd" ["27,str"]="r VAR ld ID rd"
        ["27,$"]="r VAR ld ID rd" ["28,ld"]="s 52" ["28,str"]="s 10" ["28,VAR"]="50"
        ["28,STR"]="51" ["29,ld"]="s 52" ["29,str"]="s 10" ["29,VAR"]="53"
        ["29,STR"]="54" ["30,ld"]="r DOC" ["30,rd"]="r DOC" ["30,elif"]="r DOC"
        ["30,else"]="r DOC" ["30,str"]="r DOC" ["30,DOC"]="55" ["31,ld"]="r ARGS"
        ["31,cl"]="r ARGS" ["31,or"]="r ARGS" ["31,and"]="r ARGS" ["31,lp"]="s 21"
        ["31,rp"]="r ARGS" ["31,ne"]="r ARGS" ["31,eq"]="r ARGS" ["31,gt"]="r ARGS"
        ["31,lt"]="r ARGS" ["31,ge"]="r ARGS" ["31,le"]="r ARGS" ["31,strgt"]="r ARGS"
        ["31,strlt"]="r ARGS" ["31,streq"]="r ARGS" ["31,strne"]="r ARGS"
        ["31,str"]="r ARGS" ["31,BOOLA"]="56" ["31,BOOL"]="22" ["31,ARGS"]="23"
        ["32,cl"]="r BOOLS UOP BOOLS" ["32,rp"]="r BOOLS UOP BOOLS" ["33,ld"]="r ARGS"
        ["33,cl"]="r ARGS" ["33,or"]="r ARGS" ["33,and"]="r ARGS" ["33,lp"]="s 58"
        ["33,rp"]="r ARGS" ["33,ne"]="r ARGS" ["33,eq"]="r ARGS" ["33,gt"]="r ARGS"
        ["33,lt"]="r ARGS" ["33,ge"]="r ARGS" ["33,le"]="r ARGS" ["33,strgt"]="r ARGS"
        ["33,strlt"]="r ARGS" ["33,streq"]="r ARGS" ["33,strne"]="r ARGS"
        ["33,str"]="r ARGS" ["33,BOOL"]="57" ["33,ARGS"]="23" ["34,rp"]="s 59"
        ["35,ld"]="r ARGS" ["35,cl"]="r ARGS" ["35,or"]="r ARGS" ["35,and"]="r ARGS"
        ["35,rp"]="r ARGS" ["35,str"]="r ARGS" ["35,ARGS"]="60"
        ["36,ld"]="r ARGS ARGS STMT" ["36,cl"]="r ARGS ARGS STMT"
        ["36,rd"]="r ARGS ARGS STMT" ["36,or"]="r ARGS ARGS STMT"
        ["36,and"]="r ARGS ARGS STMT" ["36,rp"]="r ARGS ARGS STMT"
        ["36,ne"]="r ARGS ARGS STMT" ["36,eq"]="r ARGS ARGS STMT"
        ["36,gt"]="r ARGS ARGS STMT" ["36,lt"]="r ARGS ARGS STMT"
        ["36,ge"]="r ARGS ARGS STMT" ["36,le"]="r ARGS ARGS STMT"
        ["36,strgt"]="r ARGS ARGS STMT" ["36,strlt"]="r ARGS ARGS STMT"
        ["36,streq"]="r ARGS ARGS STMT" ["36,strne"]="r ARGS ARGS STMT"
        ["36,str"]="r ARGS ARGS STMT" ["37,ld"]="r BOP ne" ["37,cl"]="r BOP ne"
        ["37,or"]="r BOP ne" ["37,and"]="r BOP ne" ["37,rp"]="r BOP ne"
        ["37,str"]="r BOP ne" ["38,ld"]="r BOP eq" ["38,cl"]="r BOP eq"
        ["38,or"]="r BOP eq" ["38,and"]="r BOP eq" ["38,rp"]="r BOP eq"
        ["38,str"]="r BOP eq" ["39,ld"]="r BOP gt" ["39,cl"]="r BOP gt"
        ["39,or"]="r BOP gt" ["39,and"]="r BOP gt" ["39,rp"]="r BOP gt"
        ["39,str"]="r BOP gt" ["40,ld"]="r BOP lt" ["40,cl"]="r BOP lt"
        ["40,or"]="r BOP lt" ["40,and"]="r BOP lt" ["40,rp"]="r BOP lt"
        ["40,str"]="r BOP lt" ["41,ld"]="r BOP ge" ["41,cl"]="r BOP ge"
        ["41,or"]="r BOP ge" ["41,and"]="r BOP ge" ["41,rp"]="r BOP ge"
        ["41,str"]="r BOP ge" ["42,ld"]="r BOP le" ["42,cl"]="r BOP le"
        ["42,or"]="r BOP le" ["42,and"]="r BOP le" ["42,rp"]="r BOP le"
        ["42,str"]="r BOP le" ["43,ld"]="r BOP strgt" ["43,cl"]="r BOP strgt"
        ["43,or"]="r BOP strgt" ["43,and"]="r BOP strgt" ["43,rp"]="r BOP strgt"
        ["43,str"]="r BOP strgt" ["44,ld"]="r BOP strlt" ["44,cl"]="r BOP strlt"
        ["44,or"]="r BOP strlt" ["44,and"]="r BOP strlt" ["44,rp"]="r BOP strlt"
        ["44,str"]="r BOP strlt" ["45,ld"]="r BOP streq" ["45,cl"]="r BOP streq"
        ["45,or"]="r BOP streq" ["45,and"]="r BOP streq" ["45,rp"]="r BOP streq"
        ["45,str"]="r BOP streq" ["46,ld"]="r BOP strne" ["46,cl"]="r BOP strne"
        ["46,or"]="r BOP strne" ["46,and"]="r BOP strne" ["46,rp"]="r BOP strne"
        ["46,str"]="r BOP strne" ["47,ld"]="r ARGS" ["47,cl"]="r ARGS"
        ["47,str"]="r ARGS" ["47,ARGS"]="61" ["48,rd"]="s 62" ["49,ld"]="s 9"
        ["49,rd"]="s 63" ["49,str"]="s 10" ["49,STMT"]="36" ["49,IF"]="3"
        ["49,FORIN"]="4" ["49,INCLUDE"]="5" ["49,BUILTIN"]="6" ["49,VAR"]="7"
        ["49,STR"]="8" ["50,rd"]="s 64" ["51,rd"]="s 65" ["52,id"]="s 15" ["52,ID"]="66"
        ["53,rd"]="s 67" ["54,rd"]="s 68" ["55,ld"]="s 9" ["55,rd"]="r ELIF"
        ["55,elif"]="r ELIF" ["55,else"]="r ELIF" ["55,str"]="s 10" ["55,STMT"]="2"
        ["55,IF"]="3" ["55,FORIN"]="4" ["55,INCLUDE"]="5" ["55,BUILTIN"]="6"
        ["55,VAR"]="7" ["55,STR"]="8" ["55,ELIF"]="69"
        ["56,cl"]="r BOOLO BOOLO or BOOLA" ["56,or"]="r BOOLO BOOLO or BOOLA"
        ["56,and"]="s 33" ["56,rp"]="r BOOLO BOOLO or BOOLA"
        ["57,cl"]="r BOOLA BOOLA and BOOL" ["57,or"]="r BOOLA BOOLA and BOOL"
        ["57,and"]="r BOOLA BOOLA and BOOL" ["57,rp"]="r BOOLA BOOLA and BOOL"
        ["58,ld"]="r ARGS" ["58,or"]="r ARGS" ["58,and"]="r ARGS" ["58,lp"]="s 21"
        ["58,rp"]="r ARGS" ["58,ne"]="r ARGS" ["58,eq"]="r ARGS" ["58,gt"]="r ARGS"
        ["58,lt"]="r ARGS" ["58,ge"]="r ARGS" ["58,le"]="r ARGS" ["58,strgt"]="r ARGS"
        ["58,strlt"]="r ARGS" ["58,streq"]="r ARGS" ["58,strne"]="r ARGS"
        ["58,ex"]="s 20" ["58,str"]="r ARGS" ["58,BOOLS"]="70" ["58,BOOLO"]="17"
        ["58,UOP"]="18" ["58,BOOLA"]="19" ["58,BOOL"]="22" ["58,ARGS"]="23"
        ["59,cl"]="r BOOLA lp BOOLS rp" ["59,or"]="r BOOLA lp BOOLS rp"
        ["59,and"]="r BOOLA lp BOOLS rp" ["59,rp"]="r BOOLA lp BOOLS rp" ["60,ld"]="s 9"
        ["60,cl"]="r BOOL ARGS BOP ARGS" ["60,or"]="r BOOL ARGS BOP ARGS"
        ["60,and"]="r BOOL ARGS BOP ARGS" ["60,rp"]="r BOOL ARGS BOP ARGS"
        ["60,str"]="s 10" ["60,STMT"]="36" ["60,IF"]="3" ["60,FORIN"]="4"
        ["60,INCLUDE"]="5" ["60,BUILTIN"]="6" ["60,VAR"]="7" ["60,STR"]="8"
        ["61,ld"]="s 9" ["61,cl"]="s 71" ["61,str"]="s 10" ["61,STMT"]="36"
        ["61,IF"]="3" ["61,FORIN"]="4" ["61,INCLUDE"]="5" ["61,BUILTIN"]="6"
        ["61,VAR"]="7" ["61,STR"]="8" ["62,ld"]="r INCLUDE ld include cl STR rd"
        ["62,cl"]="r INCLUDE ld include cl STR rd"
        ["62,rd"]="r INCLUDE ld include cl STR rd"
        ["62,elif"]="r INCLUDE ld include cl STR rd"
        ["62,else"]="r INCLUDE ld include cl STR rd"
        ["62,or"]="r INCLUDE ld include cl STR rd"
        ["62,and"]="r INCLUDE ld include cl STR rd"
        ["62,rp"]="r INCLUDE ld include cl STR rd"
        ["62,ne"]="r INCLUDE ld include cl STR rd"
        ["62,eq"]="r INCLUDE ld include cl STR rd"
        ["62,gt"]="r INCLUDE ld include cl STR rd"
        ["62,lt"]="r INCLUDE ld include cl STR rd"
        ["62,ge"]="r INCLUDE ld include cl STR rd"
        ["62,le"]="r INCLUDE ld include cl STR rd"
        ["62,strgt"]="r INCLUDE ld include cl STR rd"
        ["62,strlt"]="r INCLUDE ld include cl STR rd"
        ["62,streq"]="r INCLUDE ld include cl STR rd"
        ["62,strne"]="r INCLUDE ld include cl STR rd"
        ["62,str"]="r INCLUDE ld include cl STR rd"
        ["62,$"]="r INCLUDE ld include cl STR rd" ["63,ld"]="r BUILTIN ld ID cl ARGS rd"
        ["63,cl"]="r BUILTIN ld ID cl ARGS rd" ["63,rd"]="r BUILTIN ld ID cl ARGS rd"
        ["63,elif"]="r BUILTIN ld ID cl ARGS rd"
        ["63,else"]="r BUILTIN ld ID cl ARGS rd" ["63,or"]="r BUILTIN ld ID cl ARGS rd"
        ["63,and"]="r BUILTIN ld ID cl ARGS rd" ["63,rp"]="r BUILTIN ld ID cl ARGS rd"
        ["63,ne"]="r BUILTIN ld ID cl ARGS rd" ["63,eq"]="r BUILTIN ld ID cl ARGS rd"
        ["63,gt"]="r BUILTIN ld ID cl ARGS rd" ["63,lt"]="r BUILTIN ld ID cl ARGS rd"
        ["63,ge"]="r BUILTIN ld ID cl ARGS rd" ["63,le"]="r BUILTIN ld ID cl ARGS rd"
        ["63,strgt"]="r BUILTIN ld ID cl ARGS rd"
        ["63,strlt"]="r BUILTIN ld ID cl ARGS rd"
        ["63,streq"]="r BUILTIN ld ID cl ARGS rd"
        ["63,strne"]="r BUILTIN ld ID cl ARGS rd"
        ["63,str"]="r BUILTIN ld ID cl ARGS rd" ["63,$"]="r BUILTIN ld ID cl ARGS rd"
        ["64,ld"]="r VAR ld ID or VAR rd" ["64,cl"]="r VAR ld ID or VAR rd"
        ["64,rd"]="r VAR ld ID or VAR rd" ["64,elif"]="r VAR ld ID or VAR rd"
        ["64,else"]="r VAR ld ID or VAR rd" ["64,or"]="r VAR ld ID or VAR rd"
        ["64,and"]="r VAR ld ID or VAR rd" ["64,rp"]="r VAR ld ID or VAR rd"
        ["64,ne"]="r VAR ld ID or VAR rd" ["64,eq"]="r VAR ld ID or VAR rd"
        ["64,gt"]="r VAR ld ID or VAR rd" ["64,lt"]="r VAR ld ID or VAR rd"
        ["64,ge"]="r VAR ld ID or VAR rd" ["64,le"]="r VAR ld ID or VAR rd"
        ["64,strgt"]="r VAR ld ID or VAR rd" ["64,strlt"]="r VAR ld ID or VAR rd"
        ["64,streq"]="r VAR ld ID or VAR rd" ["64,strne"]="r VAR ld ID or VAR rd"
        ["64,str"]="r VAR ld ID or VAR rd" ["64,$"]="r VAR ld ID or VAR rd"
        ["65,ld"]="r VAR ld ID or STR rd" ["65,cl"]="r VAR ld ID or STR rd"
        ["65,rd"]="r VAR ld ID or STR rd" ["65,elif"]="r VAR ld ID or STR rd"
        ["65,else"]="r VAR ld ID or STR rd" ["65,or"]="r VAR ld ID or STR rd"
        ["65,and"]="r VAR ld ID or STR rd" ["65,rp"]="r VAR ld ID or STR rd"
        ["65,ne"]="r VAR ld ID or STR rd" ["65,eq"]="r VAR ld ID or STR rd"
        ["65,gt"]="r VAR ld ID or STR rd" ["65,lt"]="r VAR ld ID or STR rd"
        ["65,ge"]="r VAR ld ID or STR rd" ["65,le"]="r VAR ld ID or STR rd"
        ["65,strgt"]="r VAR ld ID or STR rd" ["65,strlt"]="r VAR ld ID or STR rd"
        ["65,streq"]="r VAR ld ID or STR rd" ["65,strne"]="r VAR ld ID or STR rd"
        ["65,str"]="r VAR ld ID or STR rd" ["65,$"]="r VAR ld ID or STR rd"
        ["66,rd"]="s 27" ["66,or"]="s 28" ["66,and"]="s 29"
        ["67,ld"]="r VAR ld ID and VAR rd" ["67,cl"]="r VAR ld ID and VAR rd"
        ["67,rd"]="r VAR ld ID and VAR rd" ["67,elif"]="r VAR ld ID and VAR rd"
        ["67,else"]="r VAR ld ID and VAR rd" ["67,or"]="r VAR ld ID and VAR rd"
        ["67,and"]="r VAR ld ID and VAR rd" ["67,rp"]="r VAR ld ID and VAR rd"
        ["67,ne"]="r VAR ld ID and VAR rd" ["67,eq"]="r VAR ld ID and VAR rd"
        ["67,gt"]="r VAR ld ID and VAR rd" ["67,lt"]="r VAR ld ID and VAR rd"
        ["67,ge"]="r VAR ld ID and VAR rd" ["67,le"]="r VAR ld ID and VAR rd"
        ["67,strgt"]="r VAR ld ID and VAR rd" ["67,strlt"]="r VAR ld ID and VAR rd"
        ["67,streq"]="r VAR ld ID and VAR rd" ["67,strne"]="r VAR ld ID and VAR rd"
        ["67,str"]="r VAR ld ID and VAR rd" ["67,$"]="r VAR ld ID and VAR rd"
        ["68,ld"]="r VAR ld ID and STR rd" ["68,cl"]="r VAR ld ID and STR rd"
        ["68,rd"]="r VAR ld ID and STR rd" ["68,elif"]="r VAR ld ID and STR rd"
        ["68,else"]="r VAR ld ID and STR rd" ["68,or"]="r VAR ld ID and STR rd"
        ["68,and"]="r VAR ld ID and STR rd" ["68,rp"]="r VAR ld ID and STR rd"
        ["68,ne"]="r VAR ld ID and STR rd" ["68,eq"]="r VAR ld ID and STR rd"
        ["68,gt"]="r VAR ld ID and STR rd" ["68,lt"]="r VAR ld ID and STR rd"
        ["68,ge"]="r VAR ld ID and STR rd" ["68,le"]="r VAR ld ID and STR rd"
        ["68,strgt"]="r VAR ld ID and STR rd" ["68,strlt"]="r VAR ld ID and STR rd"
        ["68,streq"]="r VAR ld ID and STR rd" ["68,strne"]="r VAR ld ID and STR rd"
        ["68,str"]="r VAR ld ID and STR rd" ["68,$"]="r VAR ld ID and STR rd"
        ["69,rd"]="r ELSE" ["69,elif"]="s 73" ["69,else"]="s 74" ["69,ELSE"]="72"
        ["70,rp"]="s 75" ["71,ld"]="r DOC" ["71,rd"]="r DOC" ["71,str"]="r DOC"
        ["71,DOC"]="76" ["72,rd"]="s 77" ["73,ld"]="r ARGS" ["73,cl"]="r ARGS"
        ["73,or"]="r ARGS" ["73,and"]="r ARGS" ["73,lp"]="s 21" ["73,ne"]="r ARGS"
        ["73,eq"]="r ARGS" ["73,gt"]="r ARGS" ["73,lt"]="r ARGS" ["73,ge"]="r ARGS"
        ["73,le"]="r ARGS" ["73,strgt"]="r ARGS" ["73,strlt"]="r ARGS"
        ["73,streq"]="r ARGS" ["73,strne"]="r ARGS" ["73,ex"]="s 20" ["73,str"]="r ARGS"
        ["73,BOOLS"]="78" ["73,BOOLO"]="17" ["73,UOP"]="18" ["73,BOOLA"]="19"
        ["73,BOOL"]="22" ["73,ARGS"]="23" ["74,cl"]="s 79"
        ["75,cl"]="r BOOLA BOOLA and lp BOOLS rp"
        ["75,or"]="r BOOLA BOOLA and lp BOOLS rp"
        ["75,and"]="r BOOLA BOOLA and lp BOOLS rp"
        ["75,rp"]="r BOOLA BOOLA and lp BOOLS rp" ["76,ld"]="s 9" ["76,rd"]="s 80"
        ["76,str"]="s 10" ["76,STMT"]="2" ["76,IF"]="3" ["76,FORIN"]="4"
        ["76,INCLUDE"]="5" ["76,BUILTIN"]="6" ["76,VAR"]="7" ["76,STR"]="8"
        ["77,ld"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,cl"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,rd"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,elif"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,else"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,or"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,and"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,rp"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,ne"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,eq"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,gt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,lt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,ge"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,le"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,strgt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,strlt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,streq"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,strne"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,str"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["77,$"]="r IF ld if BOOLS cl DOC ELIF ELSE rd" ["78,cl"]="s 81"
        ["79,ld"]="r DOC" ["79,rd"]="r DOC" ["79,str"]="r DOC" ["79,DOC"]="82"
        ["80,ld"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,cl"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,rd"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,elif"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,else"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,or"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,and"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,rp"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,ne"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,eq"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,gt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,lt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,ge"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,le"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,strgt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,strlt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,streq"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,strne"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,str"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["80,$"]="r FORIN ld for ID in ARGS cl DOC rd" ["81,ld"]="r DOC"
        ["81,rd"]="r DOC" ["81,elif"]="r DOC" ["81,else"]="r DOC" ["81,str"]="r DOC"
        ["81,DOC"]="83" ["82,ld"]="s 9" ["82,rd"]="r ELSE else cl DOC" ["82,str"]="s 10"
        ["82,STMT"]="2" ["82,IF"]="3" ["82,FORIN"]="4" ["82,INCLUDE"]="5"
        ["82,BUILTIN"]="6" ["82,VAR"]="7" ["82,STR"]="8" ["83,ld"]="s 9"
        ["83,rd"]="r ELIF ELIF elif BOOLS cl DOC"
        ["83,elif"]="r ELIF ELIF elif BOOLS cl DOC"
        ["83,else"]="r ELIF ELIF elif BOOLS cl DOC" ["83,str"]="s 10" ["83,STMT"]="2"
        ["83,IF"]="3" ["83,FORIN"]="4" ["83,INCLUDE"]="5" ["83,BUILTIN"]="6"
        ["83,VAR"]="7" ["83,STR"]="8"
    ) # }}}
    # <<< BPT_PARSE_TABLE_E <<<

    # Deduplication function for collect-{var,include}
    bpt.__dedup() { echo "$1" | sort | uniq; }

    # Append this if reducer is bpt.__reduce_generate
    local HEADER=''
    [[ $reduce_fn != bpt.__reduce_generate ]] || {
        read -r -d '' HEADER <<-'EOF'
#!/bin/bash
e(){ local OIFS="$IFS"; IFS=; echo -n "$*"; IFS="$OIFS"; };
len(){ echo -n "${#1}"; };
EOF
    }

    # Execute command
    case "$cmd" in
    scan) bpt.scan "$ld" "$rd" <"$infile" ;;
    fingerprint) bpt.fingerprint "$ld" "$rd" "$infile" "$debug" ;;
    *) result="$(bpt.process "$ld" "$rd" "$reduce_fn" "$infile" "$debug")" &&
        $post_process "$HEADER$result" ;;
    esac
}

(return 0 2>/dev/null) || bpt.main "$@"
