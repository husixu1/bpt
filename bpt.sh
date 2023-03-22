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
    BOOL)
        case "${#rule[@]}" in
        3) contents[$s]="${contents[$((s + 1))]}" ;;
        4) contents[$s]+="${contents[$((s + 2))]}" ;;
        esac
        ;;
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
        quote) contents[$s]="\"${contents[$((s + 3))]}\"" ;;
        *) echo "Unrecognized builtin function ${contents[$((s + 1))]}" >&2 && exit 1 ;;
        esac
        ;;
    INCLUDE) contents[$s]="$(__recursive_process "${contents[$((s + 3))]}")" ;;
    FORIN) contents[$s]="for ${contents[$((s + 2))]} in ${contents[$((s + 4))]}; do ${contents[$((s + 6))]} done" ;;
    BOOL)
        case "${#rule[@]}" in
        3) contents[$s]="${contents[$s]}${contents[$((s + 1))]}" ;;
        *)
            local -a rhss=()
            # Strip the tag from STMT
            local stmt_l_type="${contents[$s]%%:*}"
            local stmt_l="${contents[$s]#*:}"
            case "$stmt_l_type" in
            ID | STR) rhss[0]="\$(e ${stmt_l@Q})" ;;
            VAR) rhss[0]="\"$stmt_l\"" ;;
            *) rhss[0]="\$(${stmt_l})" ;;
            esac
            # If rule is STMT op STMT, deal with op and RHS
            [[ ${#rule[@]} -eq 2 ]] || {
                rhss[1]="${contents[$((s + 1))]}"
                local stmt_r_type="${contents[$((s + 2))]%%:*}"
                local stmt_r="${contents[$((s + 2))]#*:}"
                case "$stmt_r_type" in
                ID | STR) rhss[2]="\$(e ${stmt_r@Q})" ;;
                VAR) rhss[2]="\"$stmt_r\"" ;;
                *) rhss[2]="\$(${stmt_r})" ;;
                esac
            }
            contents[$s]="${rhss[*]}"
            ;;
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
    ELSE)
        case "${#rule[@]}" in
        1) contents[$s]='' ;;
        *) contents[$s]="else ${contents[$((s + 2))]}" ;;
        esac
        ;;
    ELIF)
        case "${#rule[@]}" in
        1) contents[$s]='' ;;
        *) contents[$s]="${contents[$s]}; elif [[ ${contents[$((s + 2))]} ]]; then ${contents[$((s + 4))]}" ;;
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
#   IF      -> ld if BOOLO cl DOC ELIF ELSE rd .
#   ELIF    -> ELIF elif BOOLO cl DOC
#            | .
#   ELSE    -> else cl DOC
#            | .
#   BOOLO   -> BOOLO or BOOLA
#            | BOOLA .
#   BOOLA   -> BOOLA and BOOL
#            | BOOLA and lp BOOLO rp
#            | lp BOOLO rp
#            | BOOL .
#   BOOL    -> STMT BOP STMT
#            | STMT
#            | UOP BOOL .
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
        ["10,str"]="r STR str" ["10,$"]="r STR str" ["11,ld"]="s 9" ["11,lp"]="s 18"
        ["11,ex"]="s 22" ["11,str"]="s 10" ["11,STMT"]="20" ["11,IF"]="3"
        ["11,FORIN"]="4" ["11,INCLUDE"]="5" ["11,BUILTIN"]="6" ["11,VAR"]="7"
        ["11,STR"]="8" ["11,BOOLO"]="16" ["11,BOOLA"]="17" ["11,BOOL"]="19"
        ["11,UOP"]="21" ["12,id"]="s 15" ["12,ID"]="23" ["13,cl"]="s 24"
        ["14,cl"]="s 25" ["14,rd"]="s 26" ["14,or"]="s 27" ["14,and"]="s 28"
        ["15,cl"]="r ID id" ["15,rd"]="r ID id" ["15,or"]="r ID id" ["15,and"]="r ID id"
        ["15,in"]="r ID id" ["16,cl"]="s 29" ["16,or"]="s 30" ["17,cl"]="r BOOLO BOOLA"
        ["17,or"]="r BOOLO BOOLA" ["17,and"]="s 31" ["17,rp"]="r BOOLO BOOLA"
        ["18,ld"]="s 9" ["18,lp"]="s 18" ["18,ex"]="s 22" ["18,str"]="s 10"
        ["18,STMT"]="20" ["18,IF"]="3" ["18,FORIN"]="4" ["18,INCLUDE"]="5"
        ["18,BUILTIN"]="6" ["18,VAR"]="7" ["18,STR"]="8" ["18,BOOLO"]="32"
        ["18,BOOLA"]="17" ["18,BOOL"]="19" ["18,UOP"]="21" ["19,cl"]="r BOOLA BOOL"
        ["19,or"]="r BOOLA BOOL" ["19,and"]="r BOOLA BOOL" ["19,rp"]="r BOOLA BOOL"
        ["20,cl"]="r BOOL STMT" ["20,or"]="r BOOL STMT" ["20,and"]="r BOOL STMT"
        ["20,rp"]="r BOOL STMT" ["20,ne"]="s 34" ["20,eq"]="s 35" ["20,gt"]="s 36"
        ["20,lt"]="s 37" ["20,ge"]="s 38" ["20,le"]="s 39" ["20,strgt"]="s 40"
        ["20,strlt"]="s 41" ["20,streq"]="s 42" ["20,strne"]="s 43" ["20,BOP"]="33"
        ["21,ld"]="s 9" ["21,ex"]="s 22" ["21,str"]="s 10" ["21,STMT"]="20"
        ["21,IF"]="3" ["21,FORIN"]="4" ["21,INCLUDE"]="5" ["21,BUILTIN"]="6"
        ["21,VAR"]="7" ["21,STR"]="8" ["21,BOOL"]="44" ["21,UOP"]="21"
        ["22,ld"]="r UOP ex" ["22,ex"]="r UOP ex" ["22,str"]="r UOP ex" ["23,in"]="s 45"
        ["24,str"]="s 10" ["24,STR"]="46" ["25,ld"]="r ARGS" ["25,rd"]="r ARGS"
        ["25,str"]="r ARGS" ["25,ARGS"]="47" ["26,ld"]="r VAR ld ID rd"
        ["26,cl"]="r VAR ld ID rd" ["26,rd"]="r VAR ld ID rd"
        ["26,elif"]="r VAR ld ID rd" ["26,else"]="r VAR ld ID rd"
        ["26,or"]="r VAR ld ID rd" ["26,and"]="r VAR ld ID rd"
        ["26,rp"]="r VAR ld ID rd" ["26,ne"]="r VAR ld ID rd" ["26,eq"]="r VAR ld ID rd"
        ["26,gt"]="r VAR ld ID rd" ["26,lt"]="r VAR ld ID rd" ["26,ge"]="r VAR ld ID rd"
        ["26,le"]="r VAR ld ID rd" ["26,strgt"]="r VAR ld ID rd"
        ["26,strlt"]="r VAR ld ID rd" ["26,streq"]="r VAR ld ID rd"
        ["26,strne"]="r VAR ld ID rd" ["26,str"]="r VAR ld ID rd"
        ["26,$"]="r VAR ld ID rd" ["27,ld"]="s 50" ["27,str"]="s 10" ["27,VAR"]="48"
        ["27,STR"]="49" ["28,ld"]="s 50" ["28,str"]="s 10" ["28,VAR"]="51"
        ["28,STR"]="52" ["29,ld"]="r DOC" ["29,rd"]="r DOC" ["29,elif"]="r DOC"
        ["29,else"]="r DOC" ["29,str"]="r DOC" ["29,DOC"]="53" ["30,ld"]="s 9"
        ["30,lp"]="s 18" ["30,ex"]="s 22" ["30,str"]="s 10" ["30,STMT"]="20"
        ["30,IF"]="3" ["30,FORIN"]="4" ["30,INCLUDE"]="5" ["30,BUILTIN"]="6"
        ["30,VAR"]="7" ["30,STR"]="8" ["30,BOOLA"]="54" ["30,BOOL"]="19" ["30,UOP"]="21"
        ["31,ld"]="s 9" ["31,lp"]="s 56" ["31,ex"]="s 22" ["31,str"]="s 10"
        ["31,STMT"]="20" ["31,IF"]="3" ["31,FORIN"]="4" ["31,INCLUDE"]="5"
        ["31,BUILTIN"]="6" ["31,VAR"]="7" ["31,STR"]="8" ["31,BOOL"]="55"
        ["31,UOP"]="21" ["32,or"]="s 30" ["32,rp"]="s 57" ["33,ld"]="s 9"
        ["33,str"]="s 10" ["33,STMT"]="58" ["33,IF"]="3" ["33,FORIN"]="4"
        ["33,INCLUDE"]="5" ["33,BUILTIN"]="6" ["33,VAR"]="7" ["33,STR"]="8"
        ["34,ld"]="r BOP ne" ["34,str"]="r BOP ne" ["35,ld"]="r BOP eq"
        ["35,str"]="r BOP eq" ["36,ld"]="r BOP gt" ["36,str"]="r BOP gt"
        ["37,ld"]="r BOP lt" ["37,str"]="r BOP lt" ["38,ld"]="r BOP ge"
        ["38,str"]="r BOP ge" ["39,ld"]="r BOP le" ["39,str"]="r BOP le"
        ["40,ld"]="r BOP strgt" ["40,str"]="r BOP strgt" ["41,ld"]="r BOP strlt"
        ["41,str"]="r BOP strlt" ["42,ld"]="r BOP streq" ["42,str"]="r BOP streq"
        ["43,ld"]="r BOP strne" ["43,str"]="r BOP strne" ["44,cl"]="r BOOL UOP BOOL"
        ["44,or"]="r BOOL UOP BOOL" ["44,and"]="r BOOL UOP BOOL"
        ["44,rp"]="r BOOL UOP BOOL" ["45,ld"]="r ARGS" ["45,cl"]="r ARGS"
        ["45,str"]="r ARGS" ["45,ARGS"]="59" ["46,rd"]="s 60" ["47,ld"]="s 9"
        ["47,rd"]="s 61" ["47,str"]="s 10" ["47,STMT"]="62" ["47,IF"]="3"
        ["47,FORIN"]="4" ["47,INCLUDE"]="5" ["47,BUILTIN"]="6" ["47,VAR"]="7"
        ["47,STR"]="8" ["48,rd"]="s 63" ["49,rd"]="s 64" ["50,id"]="s 15" ["50,ID"]="65"
        ["51,rd"]="s 66" ["52,rd"]="s 67" ["53,ld"]="s 9" ["53,rd"]="r ELIF"
        ["53,elif"]="r ELIF" ["53,else"]="r ELIF" ["53,str"]="s 10" ["53,STMT"]="2"
        ["53,IF"]="3" ["53,FORIN"]="4" ["53,INCLUDE"]="5" ["53,BUILTIN"]="6"
        ["53,VAR"]="7" ["53,STR"]="8" ["53,ELIF"]="68"
        ["54,cl"]="r BOOLO BOOLO or BOOLA" ["54,or"]="r BOOLO BOOLO or BOOLA"
        ["54,and"]="s 31" ["54,rp"]="r BOOLO BOOLO or BOOLA"
        ["55,cl"]="r BOOLA BOOLA and BOOL" ["55,or"]="r BOOLA BOOLA and BOOL"
        ["55,and"]="r BOOLA BOOLA and BOOL" ["55,rp"]="r BOOLA BOOLA and BOOL"
        ["56,ld"]="s 9" ["56,lp"]="s 18" ["56,ex"]="s 22" ["56,str"]="s 10"
        ["56,STMT"]="20" ["56,IF"]="3" ["56,FORIN"]="4" ["56,INCLUDE"]="5"
        ["56,BUILTIN"]="6" ["56,VAR"]="7" ["56,STR"]="8" ["56,BOOLO"]="69"
        ["56,BOOLA"]="17" ["56,BOOL"]="19" ["56,UOP"]="21"
        ["57,cl"]="r BOOLA lp BOOLO rp" ["57,or"]="r BOOLA lp BOOLO rp"
        ["57,and"]="r BOOLA lp BOOLO rp" ["57,rp"]="r BOOLA lp BOOLO rp"
        ["58,cl"]="r BOOL STMT BOP STMT" ["58,or"]="r BOOL STMT BOP STMT"
        ["58,and"]="r BOOL STMT BOP STMT" ["58,rp"]="r BOOL STMT BOP STMT"
        ["59,ld"]="s 9" ["59,cl"]="s 70" ["59,str"]="s 10" ["59,STMT"]="62"
        ["59,IF"]="3" ["59,FORIN"]="4" ["59,INCLUDE"]="5" ["59,BUILTIN"]="6"
        ["59,VAR"]="7" ["59,STR"]="8" ["60,ld"]="r INCLUDE ld include cl STR rd"
        ["60,cl"]="r INCLUDE ld include cl STR rd"
        ["60,rd"]="r INCLUDE ld include cl STR rd"
        ["60,elif"]="r INCLUDE ld include cl STR rd"
        ["60,else"]="r INCLUDE ld include cl STR rd"
        ["60,or"]="r INCLUDE ld include cl STR rd"
        ["60,and"]="r INCLUDE ld include cl STR rd"
        ["60,rp"]="r INCLUDE ld include cl STR rd"
        ["60,ne"]="r INCLUDE ld include cl STR rd"
        ["60,eq"]="r INCLUDE ld include cl STR rd"
        ["60,gt"]="r INCLUDE ld include cl STR rd"
        ["60,lt"]="r INCLUDE ld include cl STR rd"
        ["60,ge"]="r INCLUDE ld include cl STR rd"
        ["60,le"]="r INCLUDE ld include cl STR rd"
        ["60,strgt"]="r INCLUDE ld include cl STR rd"
        ["60,strlt"]="r INCLUDE ld include cl STR rd"
        ["60,streq"]="r INCLUDE ld include cl STR rd"
        ["60,strne"]="r INCLUDE ld include cl STR rd"
        ["60,str"]="r INCLUDE ld include cl STR rd"
        ["60,$"]="r INCLUDE ld include cl STR rd" ["61,ld"]="r BUILTIN ld ID cl ARGS rd"
        ["61,cl"]="r BUILTIN ld ID cl ARGS rd" ["61,rd"]="r BUILTIN ld ID cl ARGS rd"
        ["61,elif"]="r BUILTIN ld ID cl ARGS rd"
        ["61,else"]="r BUILTIN ld ID cl ARGS rd" ["61,or"]="r BUILTIN ld ID cl ARGS rd"
        ["61,and"]="r BUILTIN ld ID cl ARGS rd" ["61,rp"]="r BUILTIN ld ID cl ARGS rd"
        ["61,ne"]="r BUILTIN ld ID cl ARGS rd" ["61,eq"]="r BUILTIN ld ID cl ARGS rd"
        ["61,gt"]="r BUILTIN ld ID cl ARGS rd" ["61,lt"]="r BUILTIN ld ID cl ARGS rd"
        ["61,ge"]="r BUILTIN ld ID cl ARGS rd" ["61,le"]="r BUILTIN ld ID cl ARGS rd"
        ["61,strgt"]="r BUILTIN ld ID cl ARGS rd"
        ["61,strlt"]="r BUILTIN ld ID cl ARGS rd"
        ["61,streq"]="r BUILTIN ld ID cl ARGS rd"
        ["61,strne"]="r BUILTIN ld ID cl ARGS rd"
        ["61,str"]="r BUILTIN ld ID cl ARGS rd" ["61,$"]="r BUILTIN ld ID cl ARGS rd"
        ["62,ld"]="r ARGS ARGS STMT" ["62,cl"]="r ARGS ARGS STMT"
        ["62,rd"]="r ARGS ARGS STMT" ["62,str"]="r ARGS ARGS STMT"
        ["63,ld"]="r VAR ld ID or VAR rd" ["63,cl"]="r VAR ld ID or VAR rd"
        ["63,rd"]="r VAR ld ID or VAR rd" ["63,elif"]="r VAR ld ID or VAR rd"
        ["63,else"]="r VAR ld ID or VAR rd" ["63,or"]="r VAR ld ID or VAR rd"
        ["63,and"]="r VAR ld ID or VAR rd" ["63,rp"]="r VAR ld ID or VAR rd"
        ["63,ne"]="r VAR ld ID or VAR rd" ["63,eq"]="r VAR ld ID or VAR rd"
        ["63,gt"]="r VAR ld ID or VAR rd" ["63,lt"]="r VAR ld ID or VAR rd"
        ["63,ge"]="r VAR ld ID or VAR rd" ["63,le"]="r VAR ld ID or VAR rd"
        ["63,strgt"]="r VAR ld ID or VAR rd" ["63,strlt"]="r VAR ld ID or VAR rd"
        ["63,streq"]="r VAR ld ID or VAR rd" ["63,strne"]="r VAR ld ID or VAR rd"
        ["63,str"]="r VAR ld ID or VAR rd" ["63,$"]="r VAR ld ID or VAR rd"
        ["64,ld"]="r VAR ld ID or STR rd" ["64,cl"]="r VAR ld ID or STR rd"
        ["64,rd"]="r VAR ld ID or STR rd" ["64,elif"]="r VAR ld ID or STR rd"
        ["64,else"]="r VAR ld ID or STR rd" ["64,or"]="r VAR ld ID or STR rd"
        ["64,and"]="r VAR ld ID or STR rd" ["64,rp"]="r VAR ld ID or STR rd"
        ["64,ne"]="r VAR ld ID or STR rd" ["64,eq"]="r VAR ld ID or STR rd"
        ["64,gt"]="r VAR ld ID or STR rd" ["64,lt"]="r VAR ld ID or STR rd"
        ["64,ge"]="r VAR ld ID or STR rd" ["64,le"]="r VAR ld ID or STR rd"
        ["64,strgt"]="r VAR ld ID or STR rd" ["64,strlt"]="r VAR ld ID or STR rd"
        ["64,streq"]="r VAR ld ID or STR rd" ["64,strne"]="r VAR ld ID or STR rd"
        ["64,str"]="r VAR ld ID or STR rd" ["64,$"]="r VAR ld ID or STR rd"
        ["65,rd"]="s 26" ["65,or"]="s 27" ["65,and"]="s 28"
        ["66,ld"]="r VAR ld ID and VAR rd" ["66,cl"]="r VAR ld ID and VAR rd"
        ["66,rd"]="r VAR ld ID and VAR rd" ["66,elif"]="r VAR ld ID and VAR rd"
        ["66,else"]="r VAR ld ID and VAR rd" ["66,or"]="r VAR ld ID and VAR rd"
        ["66,and"]="r VAR ld ID and VAR rd" ["66,rp"]="r VAR ld ID and VAR rd"
        ["66,ne"]="r VAR ld ID and VAR rd" ["66,eq"]="r VAR ld ID and VAR rd"
        ["66,gt"]="r VAR ld ID and VAR rd" ["66,lt"]="r VAR ld ID and VAR rd"
        ["66,ge"]="r VAR ld ID and VAR rd" ["66,le"]="r VAR ld ID and VAR rd"
        ["66,strgt"]="r VAR ld ID and VAR rd" ["66,strlt"]="r VAR ld ID and VAR rd"
        ["66,streq"]="r VAR ld ID and VAR rd" ["66,strne"]="r VAR ld ID and VAR rd"
        ["66,str"]="r VAR ld ID and VAR rd" ["66,$"]="r VAR ld ID and VAR rd"
        ["67,ld"]="r VAR ld ID and STR rd" ["67,cl"]="r VAR ld ID and STR rd"
        ["67,rd"]="r VAR ld ID and STR rd" ["67,elif"]="r VAR ld ID and STR rd"
        ["67,else"]="r VAR ld ID and STR rd" ["67,or"]="r VAR ld ID and STR rd"
        ["67,and"]="r VAR ld ID and STR rd" ["67,rp"]="r VAR ld ID and STR rd"
        ["67,ne"]="r VAR ld ID and STR rd" ["67,eq"]="r VAR ld ID and STR rd"
        ["67,gt"]="r VAR ld ID and STR rd" ["67,lt"]="r VAR ld ID and STR rd"
        ["67,ge"]="r VAR ld ID and STR rd" ["67,le"]="r VAR ld ID and STR rd"
        ["67,strgt"]="r VAR ld ID and STR rd" ["67,strlt"]="r VAR ld ID and STR rd"
        ["67,streq"]="r VAR ld ID and STR rd" ["67,strne"]="r VAR ld ID and STR rd"
        ["67,str"]="r VAR ld ID and STR rd" ["67,$"]="r VAR ld ID and STR rd"
        ["68,rd"]="r ELSE" ["68,elif"]="s 72" ["68,else"]="s 73" ["68,ELSE"]="71"
        ["69,or"]="s 30" ["69,rp"]="s 74" ["70,ld"]="r DOC" ["70,rd"]="r DOC"
        ["70,str"]="r DOC" ["70,DOC"]="75" ["71,rd"]="s 76" ["72,ld"]="s 9"
        ["72,lp"]="s 18" ["72,ex"]="s 22" ["72,str"]="s 10" ["72,STMT"]="20"
        ["72,IF"]="3" ["72,FORIN"]="4" ["72,INCLUDE"]="5" ["72,BUILTIN"]="6"
        ["72,VAR"]="7" ["72,STR"]="8" ["72,BOOLO"]="77" ["72,BOOLA"]="17"
        ["72,BOOL"]="19" ["72,UOP"]="21" ["73,cl"]="s 78"
        ["74,cl"]="r BOOLA BOOLA and lp BOOLO rp"
        ["74,or"]="r BOOLA BOOLA and lp BOOLO rp"
        ["74,and"]="r BOOLA BOOLA and lp BOOLO rp"
        ["74,rp"]="r BOOLA BOOLA and lp BOOLO rp" ["75,ld"]="s 9" ["75,rd"]="s 79"
        ["75,str"]="s 10" ["75,STMT"]="2" ["75,IF"]="3" ["75,FORIN"]="4"
        ["75,INCLUDE"]="5" ["75,BUILTIN"]="6" ["75,VAR"]="7" ["75,STR"]="8"
        ["76,ld"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,cl"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,rd"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,elif"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,else"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,or"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,and"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,rp"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,ne"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,eq"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,gt"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,lt"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,ge"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,le"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,strgt"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,strlt"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,streq"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,strne"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,str"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["76,$"]="r IF ld if BOOLO cl DOC ELIF ELSE rd" ["77,cl"]="s 80"
        ["77,or"]="s 30" ["78,ld"]="r DOC" ["78,rd"]="r DOC" ["78,str"]="r DOC"
        ["78,DOC"]="81" ["79,ld"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,cl"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,rd"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,elif"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,else"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,or"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,and"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,rp"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,ne"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,eq"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,gt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,lt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,ge"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,le"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,strgt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,strlt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,streq"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,strne"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,str"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["79,$"]="r FORIN ld for ID in ARGS cl DOC rd" ["80,ld"]="r DOC"
        ["80,rd"]="r DOC" ["80,elif"]="r DOC" ["80,else"]="r DOC" ["80,str"]="r DOC"
        ["80,DOC"]="82" ["81,ld"]="s 9" ["81,rd"]="r ELSE else cl DOC" ["81,str"]="s 10"
        ["81,STMT"]="2" ["81,IF"]="3" ["81,FORIN"]="4" ["81,INCLUDE"]="5"
        ["81,BUILTIN"]="6" ["81,VAR"]="7" ["81,STR"]="8" ["82,ld"]="s 9"
        ["82,rd"]="r ELIF ELIF elif BOOLO cl DOC"
        ["82,elif"]="r ELIF ELIF elif BOOLO cl DOC"
        ["82,else"]="r ELIF ELIF elif BOOLO cl DOC" ["82,str"]="s 10" ["82,STMT"]="2"
        ["82,IF"]="3" ["82,FORIN"]="4" ["82,INCLUDE"]="5" ["82,BUILTIN"]="6"
        ["82,VAR"]="7" ["82,STR"]="8"
    ) # }}}
    # <<< BPT_PARSE_TABLE_E <<<

    # Deduplication function for collect-{var,include}
    bpt.__dedup() { echo "$1" | sort | uniq; }

    # Append this if reducer is bpt.__reduce_generate
    local HEADER=''
    [[ $reduce_fn != bpt.__reduce_generate ]] || {
        read -r -d '' HEADER <<-'EOF'
#!/bin/bash
e(){ echo -n "$@"; };
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
