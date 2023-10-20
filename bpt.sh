#!/bin/bash
# vim: set foldlevel=0:
# shellcheck disable=SC2317

# Import once
if [[ -n $__BPT_VERSION ]]; then return; fi
readonly __BPT_VERSION="v0.1"

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
    local line='' content='' newline=true
    __start_string
    while IFS= read -r line || { newline=false && false; } || [[ $line ]]; do
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

        # Decide whether currently scanning a string
        # Only count newlines in strings (outside `ld ... rd` and inside quotes).
        [[ $num_ld -gt 0 && -z "$quote" ]] || {
            __start_string
            ! $newline || { string+=$'\n' && ((++str_lines)); }
        }

        newline=true
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
    # For all `id` token, allow only the path via the VAR rule.
    # For all `str` token, allow only the path to the INCLUDE rule.
    case "${rule[0]}" in
    ID | STR) ;;
    STMT) [[ "${rule[1]}" != STR ]] || contents[$s]='' ;;
    VAR)
        contents[$s]="${contents[$((s + 1))]}"$'\n'
        [[ ${#rule[@]} -eq 4 || ${rule[4]} != VAR ]] ||
            contents[$s]+="${contents[$((s + 3))]}"
        ;;
    BUILTIN) contents[$s]="${contents[$((s + 3))]}" ;;
    INCLUDE) contents[$s]="$(__recursive_process "${contents[$((s + 3))]}")"$'\n' ;;
    FORIN) # Filter tokens defined by the FORIN rule
        contents[$s]="${contents[$((s + 4))]}"
        local var
        while read -r var; do
            [[ -z $var || $var == "${contents[$((s + 2))]}" ]] || contents[$s]+="$var"$'\n'
        done <<<"${contents[$((s + 6))]}"
        ;;
    *) # Prevent the propagation of all other non-terminals
        [[ "${#rule[@]}" -ne 1 ]] || { contents[$s]='' && return; }
        [[ "${rule[1]^^}" == "${rule[1]}" ]] || contents[$s]=''
        local i=1
        for (( ; i < ${#rule[@]}; ++i)); do
            [[ "${rule[i + 1],,}" == "${rule[i + 1]}" ]] ||
                contents[$s]+="${contents[$((s + i))]}"
        done
        ;;
    esac
}

# The reduce function to collect all includes
# shellcheck disable=SC2031
bpt.__reduce_collect_includes() {
    # For all `str` token, allow only the path via the INCLUDE rule.
    case "${rule[0]}" in
    STR) ;; # Allow the propagation of str
    STMT) [[ "${rule[1]}" != STR ]] || contents[$s]='' ;;
    VAR) contents[$s]='' ;;
    INCLUDE)
        contents[$s]="${contents[$((s + 3))]}"$'\n'
        contents[$s]+="$(__recursive_process "${contents[$((s + 3))]}")"
        ;;
    *) # Prevent the propagation of all other non-terminals
        [[ "${#rule[@]}" -ne 1 ]] || { contents[$s]='' && return; }
        [[ "${rule[1]^^}" == "${rule[1]}" ]] || contents[$s]=''
        local i=1
        for (( ; i < ${#rule[@]}; ++i)); do
            [[ "${rule[i + 1],,}" == "${rule[i + 1]}" ]] ||
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
    STR | UOP | BOP) ;;
    # Tag location for BUILTIN error reporting
    ID) contents[$s]+=":$num_lines:$num_bytes" ;;
    VAR)
        case "${rule[3]}" in
        rd) contents[$s]="\${${contents[$((s + 1))]%:*:*}}" ;;
        or) contents[$s]="\${${contents[$((s + 1))]%:*:*}:-\$(e " ;;&
        and) contents[$s]="\${${contents[$((s + 1))]%:*:*}:+\$(e " ;;&
        *) case "${rule[4]}" in
            VAR) contents[$s]+="\"${contents[$((s + 3))]}\")}" ;;
            STR) contents[$s]+="${contents[$((s + 3))]@Q})}" ;;
            esac ;;
        esac
        ;;
    ARGS)
        # Strip the tag from STMT
        local stmt_type='' stmt=
        case "${#rule[@]}" in
        2)
            stmt_type="${contents[$s]%%:*}"
            stmt="${contents[$s]#*:}"
            contents[$s]=''
            ;;
        3)
            stmt_type="${contents[$((s + 1))]%%:*}"
            stmt="${contents[$((s + 1))]#*:}"
            ;;
        esac

        # Note: `${stmt@Q}` is faster than `printf '%q' ${stmt}`
        case "$stmt_type" in
        STR) contents[$s]+=" ${stmt@Q} " ;;
        VAR | BUILTIN) contents[$s]+=" $stmt " ;;
        INCLUDE | FORIN | IF) contents[$s]+=" \"\$($stmt)\" " ;;
        esac
        ;;
    BUILTIN)
        # Filter allowed builtints
        local builtin_name=${contents[$((s + 1))]%:*:*}
        case "$builtin_name" in
        len | seq | split) contents[$s]="\$($builtin_name ${contents[$((s + 3))]})" ;;
        quote) contents[$s]="\"\$(e ${contents[$((s + 3))]})\"" ;;
        *) # Extract and compute correct error location from ID
            local line_col="${contents[$((s + 1))]#"$builtin_name"}"
            local err_line=${line_col%:*} && err_line="${err_line:1}"
            local err_byte=$((${line_col##*:} - ${#builtin_name}))
            $error_fn "$err_line" "$err_byte" \
                "Error Unrecognized builtin function $builtin_name" >&2
            exit 1
            ;;
        esac
        ;;
    INCLUDE) contents[$s]="$(__recursive_process "${contents[$((s + 3))]}")" ;;
    FORIN) contents[$s]="for ${contents[$((s + 2))]%:*:*} in ${contents[$((s + 4))]}; do ${contents[$((s + 6))]} done" ;;
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
        5) contents[$s]="${contents[$s]} && ${contents[$((s + 2))]} ${contents[$((s + 3))]}" ;;
        6) contents[$s]="${contents[$s]} && ( ${contents[$((s + 3))]} )" ;;
        7) contents[$s]="${contents[$s]} && ${contents[$((s + 2))]} ( ${contents[$((s + 4))]} )" ;;
        esac
        ;;
    BOOLO)
        case "${#rule[@]}" in
        4) contents[$s]="${contents[$s]} || ${contents[$((s + 2))]}" ;;
        5) contents[$s]="${contents[$s]} || ${contents[$((s + 2))]} ${contents[$((s + 3))]}" ;;
        esac
        ;;
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
        STR) case "$stmt" in
            '') contents[$s]+=":;" ;;
            *) contents[$s]+="{ e ${stmt@Q}; };" ;;
            esac ;;
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
#            | BOOLO or UOP BOOLA
#            | BOOLA .
#   BOOLA   -> BOOLA and BOOL
#            | BOOLA and UOP BOOL
#            | BOOLA and lp BOOLS rp
#            | BOOLA and UOP lp BOOLS rp
#            | lp BOOLS rp
#            | BOOL .
#   BOOL    -> ARGS BOP ARGS
#            | ARGS .
#   FORIN   -> ld for ID in ARGS cl DOC rd .
#   INCLUDE -> ld include cl STR rd .
#   BUILTIN -> ld ID cl ARGS rd .
#   ARGS    -> ARGS STMT
#            | STMT .
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
                printf "Error: cyclic include detected:\n"
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
    local var_list inc_list
    var_list="$(bpt.process "$ld" "$rd" bpt.__reduce_collect_vars "$infile" "$debug")" || exit 1
    var_list="$(bpt.__dedup "$var_list")"
    [[ -z $var_list ]] || mapfile -t vars <<<"$var_list"
    inc_list="$(bpt.process "$ld" "$rd" bpt.__reduce_collect_includes "$infile" "$debug")" || exit 1
    inc_list="$(bpt.__dedup "$inc_list")"
    [[ -z $inc_list ]] || mapfile -t incs <<<"$inc_list"
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
        if [[ -z ${!var+.} ]]; then
            echo "Error: variable '$var' is required but not set" >&2
            return 1
        fi
        md5=($(echo -n "${var}${!var}" | md5sum)) && fingerprint+=":V:${md5[0]}"
    done

    # Digest the digests
    [[ $debug ]] && echo "[DBG] Raw fingerprint: $fingerprint"
    md5=($(echo -n "${fingerprint}" | md5sum)) && fingerprint="${md5[0]}"
    echo "$fingerprint"
}

bpt.print_help() {
    echo -e "\033[1mbpt - A command-line tool for processing simple templates\033[0m"
    echo
    echo -e "\033[1mSYNOPSIS\033[0m"
    echo "  bpt <command> [-l <LEFT_DELIMITER>] [-r <RIGHT_DELIMITER>] [-d] [<FILENAME>]"
    echo
    echo -e "\033[1mCOMMANDS\033[0m"
    echo "  scan, s:"
    echo "    Call the scanner (lexer)."
    echo
    echo "  generate, g:"
    echo "    Generate a shell script based on the input file. Output is sent to stdout."
    echo
    echo "  generate-eval, ge:"
    echo "    Same as generate, but the output is evaluated. Output is sent to stdout."
    echo
    echo "  collect-vars, cv:"
    echo "    Collect variable used in the input file recursively and output them to stdout."
    echo
    echo "  collect-includes, ci:"
    echo "    Collect all files included in the input file recursively and output them to stdout."
    echo
    echo "  fingerprint, f:"
    echo "    Generate a unique identifier based on all factors affecting the evaluation output."
    echo
    echo "  -h, --help:"
    echo "    Print this help."
    echo
    echo "  -v, --version:"
    echo "    Print version number."
    echo
    echo -e "\033[1mOPTIONS\033[0m"
    echo "  -l <LEFT_DELIMITER>, --left-delimiter <LEFT_DELIMITER>:"
    echo "    Set the left delimiter to use for placeholders (default \`{{\`)."
    echo
    echo "  -r <RIGHT_DELIMITER>, --right-delimiter <RIGHT_DELIMITER>:"
    echo "    Set the right delimiter to use for placeholders (default \`}}\`)."
    echo
    echo "  -d, --debug:"
    echo "    Enable debug mode."
    echo
    echo -e "\033[1mARGUMENTS\033[0m"
    echo "   bpt takes an optional input file path as its argument. If no input file is specified, bpt will read from stdin."
    echo
    echo -e "\033[1mEXAMPLES\033[0m"
    echo "  Generate script from a single input file using default delimiters:"
    echo "    bpt g input.tpl > output.sh"
    echo
    echo "  Render the input file:"
    echo "    var1=VAR1 var2=VAR2 ... bpt ge input.tpl"
    echo
    echo "  Collect variable names and values from an input file:"
    echo "    bpt cv input.tpl"
    echo
    echo "  Collect include file paths from an input file:"
    echo "    bpt ci input.tpl"
    echo
    echo "  Generate a fingerprint for an input"
    echo "    var1=VAR1 var2=VAR2 ... bpt f input.tpl"
    echo
    echo "  Using custom delimiters:"
    echo "    bpt -l \"<<\" -r \">>\" g input.tpl > output.sh"
    echo
    echo -e "\033[1mTEMPLATE GRAMMAR EXAMPLES\033[0m"
    echo "  Variable replacements"
    echo '    {{ var }}'
    echo '    {{ var or "abc" }}'
    echo '    {{ var or {{var2}} }}'
    echo
    echo "  Branching"
    echo '    {{ if {{x}}: {{var1}} else : {{var2}} }}'
    echo '    {{ if {{x}} -gt "5": {{var1}} elif: {{var2}} else: {{var3}} }}'
    echo '    {{ if ({{var1}} > "abc" and {{var2}} < "def") or {{var3}} == "hello" : {{ include : "input2.tpl" }} }}'
    echo
    echo "    Available operators are: "
    echo "      compare numbers:   -ne, -eq, -gt, -lt, -ge, -le "
    echo "      compare strings:   >, <, ==, !="
    echo "      logical operators: and, or, !"
    echo "      grouping:          ()"
    echo
    echo "  Looping"
    echo '    {{ for {{i}} in "a" "b" "c": "abc"{{i}}"def" }}'
    echo '    {{ for {{i}} in {{seq: "5"}}: "abc"{{i}}"def" }}'
    echo
    echo "  Include another template"
    echo '    {{ include : "input2.tpl" }}'
    echo
    echo "  Builtin functions"
    echo '    {{ seq: "5" }}'
    echo '    {{ len: "abc" }}'
    echo '    {{ quote: {{seq: "1" "2" "5"}} }}'
    echo '    {{ split: "1 2 3" }}'
    echo
    echo "  Note: bpt doesn't distinguish between strings and numbers."
    echo "    All non-keywords should be treated as strings."
    echo "    All strings inside {{...}} need to be quoted. e.g. 'abc', \"abc\", '123'."
    echo
    echo -e "\033[1mCOPYRIGHT\033[0m"
    echo "  MIT License."
    echo "  Copyright (c) 2023 Hu Sixu."
    echo "  https://github.com/husixu1/bpt"
}

bpt.main() (
    # Clean the environment to avoid builtin overrides
    # See https://unix.stackexchange.com/questions/188327
    POSIXLY_CORRECT=1
    \unset -f help read unset
    \unset POSIXLY_CORRECT
    while \read -r cmd; do
        [[ "$cmd" =~ ^([a-z:.\[]+): ]] && \unset -f "${BASH_REMATCH[1]}"
    done < <(\help -s "*")

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
    -v | --version) echo "$__BPT_VERSION" && exit 0 ;;
    -h | --help | '') bpt.print_help && exit 0 ;;
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
        -h | --help)
            bpt.print_help
            exit 0
            ;;
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
        # shellcheck disable=SC2064
        trap "rm -f -- \"${infile:?}\"; trap - RETURN" RETURN
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
        ["10,str"]="r STR str" ["10,$"]="r STR str" ["11,ld"]="s 9" ["11,lp"]="s 21"
        ["11,ex"]="s 20" ["11,str"]="s 10" ["11,STMT"]="24" ["11,IF"]="3"
        ["11,FORIN"]="4" ["11,INCLUDE"]="5" ["11,BUILTIN"]="6" ["11,VAR"]="7"
        ["11,STR"]="8" ["11,BOOLS"]="16" ["11,BOOLO"]="17" ["11,UOP"]="18"
        ["11,BOOLA"]="19" ["11,BOOL"]="22" ["11,ARGS"]="23" ["12,id"]="s 15"
        ["12,ID"]="25" ["13,cl"]="s 26" ["14,cl"]="s 27" ["14,rd"]="s 28"
        ["14,or"]="s 29" ["14,and"]="s 30" ["15,cl"]="r ID id" ["15,rd"]="r ID id"
        ["15,or"]="r ID id" ["15,and"]="r ID id" ["15,in"]="r ID id" ["16,cl"]="s 31"
        ["17,cl"]="r BOOLS BOOLO" ["17,or"]="s 32" ["17,rp"]="r BOOLS BOOLO"
        ["18,ld"]="s 9" ["18,lp"]="s 21" ["18,ex"]="s 20" ["18,str"]="s 10"
        ["18,STMT"]="24" ["18,IF"]="3" ["18,FORIN"]="4" ["18,INCLUDE"]="5"
        ["18,BUILTIN"]="6" ["18,VAR"]="7" ["18,STR"]="8" ["18,BOOLS"]="33"
        ["18,BOOLO"]="17" ["18,UOP"]="18" ["18,BOOLA"]="19" ["18,BOOL"]="22"
        ["18,ARGS"]="23" ["19,cl"]="r BOOLO BOOLA" ["19,or"]="r BOOLO BOOLA"
        ["19,and"]="s 34" ["19,rp"]="r BOOLO BOOLA" ["20,ld"]="r UOP ex"
        ["20,lp"]="r UOP ex" ["20,ex"]="r UOP ex" ["20,str"]="r UOP ex" ["21,ld"]="s 9"
        ["21,lp"]="s 21" ["21,ex"]="s 20" ["21,str"]="s 10" ["21,STMT"]="24"
        ["21,IF"]="3" ["21,FORIN"]="4" ["21,INCLUDE"]="5" ["21,BUILTIN"]="6"
        ["21,VAR"]="7" ["21,STR"]="8" ["21,BOOLS"]="35" ["21,BOOLO"]="17"
        ["21,UOP"]="18" ["21,BOOLA"]="19" ["21,BOOL"]="22" ["21,ARGS"]="23"
        ["22,cl"]="r BOOLA BOOL" ["22,or"]="r BOOLA BOOL" ["22,and"]="r BOOLA BOOL"
        ["22,rp"]="r BOOLA BOOL" ["23,ld"]="s 9" ["23,cl"]="r BOOL ARGS"
        ["23,or"]="r BOOL ARGS" ["23,and"]="r BOOL ARGS" ["23,rp"]="r BOOL ARGS"
        ["23,ne"]="s 38" ["23,eq"]="s 39" ["23,gt"]="s 40" ["23,lt"]="s 41"
        ["23,ge"]="s 42" ["23,le"]="s 43" ["23,strgt"]="s 44" ["23,strlt"]="s 45"
        ["23,streq"]="s 46" ["23,strne"]="s 47" ["23,str"]="s 10" ["23,STMT"]="37"
        ["23,IF"]="3" ["23,FORIN"]="4" ["23,INCLUDE"]="5" ["23,BUILTIN"]="6"
        ["23,VAR"]="7" ["23,STR"]="8" ["23,BOP"]="36" ["24,ld"]="r ARGS STMT"
        ["24,cl"]="r ARGS STMT" ["24,rd"]="r ARGS STMT" ["24,or"]="r ARGS STMT"
        ["24,and"]="r ARGS STMT" ["24,rp"]="r ARGS STMT" ["24,ne"]="r ARGS STMT"
        ["24,eq"]="r ARGS STMT" ["24,gt"]="r ARGS STMT" ["24,lt"]="r ARGS STMT"
        ["24,ge"]="r ARGS STMT" ["24,le"]="r ARGS STMT" ["24,strgt"]="r ARGS STMT"
        ["24,strlt"]="r ARGS STMT" ["24,streq"]="r ARGS STMT" ["24,strne"]="r ARGS STMT"
        ["24,str"]="r ARGS STMT" ["25,in"]="s 48" ["26,str"]="s 10" ["26,STR"]="49"
        ["27,ld"]="s 9" ["27,str"]="s 10" ["27,STMT"]="24" ["27,IF"]="3"
        ["27,FORIN"]="4" ["27,INCLUDE"]="5" ["27,BUILTIN"]="6" ["27,VAR"]="7"
        ["27,STR"]="8" ["27,ARGS"]="50" ["28,ld"]="r VAR ld ID rd"
        ["28,cl"]="r VAR ld ID rd" ["28,rd"]="r VAR ld ID rd"
        ["28,elif"]="r VAR ld ID rd" ["28,else"]="r VAR ld ID rd"
        ["28,or"]="r VAR ld ID rd" ["28,and"]="r VAR ld ID rd"
        ["28,rp"]="r VAR ld ID rd" ["28,ne"]="r VAR ld ID rd" ["28,eq"]="r VAR ld ID rd"
        ["28,gt"]="r VAR ld ID rd" ["28,lt"]="r VAR ld ID rd" ["28,ge"]="r VAR ld ID rd"
        ["28,le"]="r VAR ld ID rd" ["28,strgt"]="r VAR ld ID rd"
        ["28,strlt"]="r VAR ld ID rd" ["28,streq"]="r VAR ld ID rd"
        ["28,strne"]="r VAR ld ID rd" ["28,str"]="r VAR ld ID rd"
        ["28,$"]="r VAR ld ID rd" ["29,ld"]="s 53" ["29,str"]="s 10" ["29,VAR"]="51"
        ["29,STR"]="52" ["30,ld"]="s 53" ["30,str"]="s 10" ["30,VAR"]="54"
        ["30,STR"]="55" ["31,ld"]="r DOC" ["31,rd"]="r DOC" ["31,elif"]="r DOC"
        ["31,else"]="r DOC" ["31,str"]="r DOC" ["31,DOC"]="56" ["32,ld"]="s 9"
        ["32,lp"]="s 21" ["32,ex"]="s 20" ["32,str"]="s 10" ["32,STMT"]="24"
        ["32,IF"]="3" ["32,FORIN"]="4" ["32,INCLUDE"]="5" ["32,BUILTIN"]="6"
        ["32,VAR"]="7" ["32,STR"]="8" ["32,UOP"]="58" ["32,BOOLA"]="57" ["32,BOOL"]="22"
        ["32,ARGS"]="23" ["33,cl"]="r BOOLS UOP BOOLS" ["33,rp"]="r BOOLS UOP BOOLS"
        ["34,ld"]="s 9" ["34,lp"]="s 61" ["34,ex"]="s 20" ["34,str"]="s 10"
        ["34,STMT"]="24" ["34,IF"]="3" ["34,FORIN"]="4" ["34,INCLUDE"]="5"
        ["34,BUILTIN"]="6" ["34,VAR"]="7" ["34,STR"]="8" ["34,UOP"]="60"
        ["34,BOOL"]="59" ["34,ARGS"]="23" ["35,rp"]="s 62" ["36,ld"]="s 9"
        ["36,str"]="s 10" ["36,STMT"]="24" ["36,IF"]="3" ["36,FORIN"]="4"
        ["36,INCLUDE"]="5" ["36,BUILTIN"]="6" ["36,VAR"]="7" ["36,STR"]="8"
        ["36,ARGS"]="63" ["37,ld"]="r ARGS ARGS STMT" ["37,cl"]="r ARGS ARGS STMT"
        ["37,rd"]="r ARGS ARGS STMT" ["37,or"]="r ARGS ARGS STMT"
        ["37,and"]="r ARGS ARGS STMT" ["37,rp"]="r ARGS ARGS STMT"
        ["37,ne"]="r ARGS ARGS STMT" ["37,eq"]="r ARGS ARGS STMT"
        ["37,gt"]="r ARGS ARGS STMT" ["37,lt"]="r ARGS ARGS STMT"
        ["37,ge"]="r ARGS ARGS STMT" ["37,le"]="r ARGS ARGS STMT"
        ["37,strgt"]="r ARGS ARGS STMT" ["37,strlt"]="r ARGS ARGS STMT"
        ["37,streq"]="r ARGS ARGS STMT" ["37,strne"]="r ARGS ARGS STMT"
        ["37,str"]="r ARGS ARGS STMT" ["38,ld"]="r BOP ne" ["38,str"]="r BOP ne"
        ["39,ld"]="r BOP eq" ["39,str"]="r BOP eq" ["40,ld"]="r BOP gt"
        ["40,str"]="r BOP gt" ["41,ld"]="r BOP lt" ["41,str"]="r BOP lt"
        ["42,ld"]="r BOP ge" ["42,str"]="r BOP ge" ["43,ld"]="r BOP le"
        ["43,str"]="r BOP le" ["44,ld"]="r BOP strgt" ["44,str"]="r BOP strgt"
        ["45,ld"]="r BOP strlt" ["45,str"]="r BOP strlt" ["46,ld"]="r BOP streq"
        ["46,str"]="r BOP streq" ["47,ld"]="r BOP strne" ["47,str"]="r BOP strne"
        ["48,ld"]="s 9" ["48,str"]="s 10" ["48,STMT"]="24" ["48,IF"]="3"
        ["48,FORIN"]="4" ["48,INCLUDE"]="5" ["48,BUILTIN"]="6" ["48,VAR"]="7"
        ["48,STR"]="8" ["48,ARGS"]="64" ["49,rd"]="s 65" ["50,ld"]="s 9"
        ["50,rd"]="s 66" ["50,str"]="s 10" ["50,STMT"]="37" ["50,IF"]="3"
        ["50,FORIN"]="4" ["50,INCLUDE"]="5" ["50,BUILTIN"]="6" ["50,VAR"]="7"
        ["50,STR"]="8" ["51,rd"]="s 67" ["52,rd"]="s 68" ["53,id"]="s 15" ["53,ID"]="69"
        ["54,rd"]="s 70" ["55,rd"]="s 71" ["56,ld"]="s 9" ["56,rd"]="r ELIF"
        ["56,elif"]="r ELIF" ["56,else"]="r ELIF" ["56,str"]="s 10" ["56,STMT"]="2"
        ["56,IF"]="3" ["56,FORIN"]="4" ["56,INCLUDE"]="5" ["56,BUILTIN"]="6"
        ["56,VAR"]="7" ["56,STR"]="8" ["56,ELIF"]="72"
        ["57,cl"]="r BOOLO BOOLO or BOOLA" ["57,or"]="r BOOLO BOOLO or BOOLA"
        ["57,and"]="s 34" ["57,rp"]="r BOOLO BOOLO or BOOLA" ["58,ld"]="s 9"
        ["58,lp"]="s 21" ["58,str"]="s 10" ["58,STMT"]="24" ["58,IF"]="3"
        ["58,FORIN"]="4" ["58,INCLUDE"]="5" ["58,BUILTIN"]="6" ["58,VAR"]="7"
        ["58,STR"]="8" ["58,BOOLA"]="73" ["58,BOOL"]="22" ["58,ARGS"]="23"
        ["59,cl"]="r BOOLA BOOLA and BOOL" ["59,or"]="r BOOLA BOOLA and BOOL"
        ["59,and"]="r BOOLA BOOLA and BOOL" ["59,rp"]="r BOOLA BOOLA and BOOL"
        ["60,ld"]="s 9" ["60,lp"]="s 75" ["60,str"]="s 10" ["60,STMT"]="24"
        ["60,IF"]="3" ["60,FORIN"]="4" ["60,INCLUDE"]="5" ["60,BUILTIN"]="6"
        ["60,VAR"]="7" ["60,STR"]="8" ["60,BOOL"]="74" ["60,ARGS"]="23" ["61,ld"]="s 9"
        ["61,lp"]="s 21" ["61,ex"]="s 20" ["61,str"]="s 10" ["61,STMT"]="24"
        ["61,IF"]="3" ["61,FORIN"]="4" ["61,INCLUDE"]="5" ["61,BUILTIN"]="6"
        ["61,VAR"]="7" ["61,STR"]="8" ["61,BOOLS"]="76" ["61,BOOLO"]="17"
        ["61,UOP"]="18" ["61,BOOLA"]="19" ["61,BOOL"]="22" ["61,ARGS"]="23"
        ["62,cl"]="r BOOLA lp BOOLS rp" ["62,or"]="r BOOLA lp BOOLS rp"
        ["62,and"]="r BOOLA lp BOOLS rp" ["62,rp"]="r BOOLA lp BOOLS rp" ["63,ld"]="s 9"
        ["63,cl"]="r BOOL ARGS BOP ARGS" ["63,or"]="r BOOL ARGS BOP ARGS"
        ["63,and"]="r BOOL ARGS BOP ARGS" ["63,rp"]="r BOOL ARGS BOP ARGS"
        ["63,str"]="s 10" ["63,STMT"]="37" ["63,IF"]="3" ["63,FORIN"]="4"
        ["63,INCLUDE"]="5" ["63,BUILTIN"]="6" ["63,VAR"]="7" ["63,STR"]="8"
        ["64,ld"]="s 9" ["64,cl"]="s 77" ["64,str"]="s 10" ["64,STMT"]="37"
        ["64,IF"]="3" ["64,FORIN"]="4" ["64,INCLUDE"]="5" ["64,BUILTIN"]="6"
        ["64,VAR"]="7" ["64,STR"]="8" ["65,ld"]="r INCLUDE ld include cl STR rd"
        ["65,cl"]="r INCLUDE ld include cl STR rd"
        ["65,rd"]="r INCLUDE ld include cl STR rd"
        ["65,elif"]="r INCLUDE ld include cl STR rd"
        ["65,else"]="r INCLUDE ld include cl STR rd"
        ["65,or"]="r INCLUDE ld include cl STR rd"
        ["65,and"]="r INCLUDE ld include cl STR rd"
        ["65,rp"]="r INCLUDE ld include cl STR rd"
        ["65,ne"]="r INCLUDE ld include cl STR rd"
        ["65,eq"]="r INCLUDE ld include cl STR rd"
        ["65,gt"]="r INCLUDE ld include cl STR rd"
        ["65,lt"]="r INCLUDE ld include cl STR rd"
        ["65,ge"]="r INCLUDE ld include cl STR rd"
        ["65,le"]="r INCLUDE ld include cl STR rd"
        ["65,strgt"]="r INCLUDE ld include cl STR rd"
        ["65,strlt"]="r INCLUDE ld include cl STR rd"
        ["65,streq"]="r INCLUDE ld include cl STR rd"
        ["65,strne"]="r INCLUDE ld include cl STR rd"
        ["65,str"]="r INCLUDE ld include cl STR rd"
        ["65,$"]="r INCLUDE ld include cl STR rd" ["66,ld"]="r BUILTIN ld ID cl ARGS rd"
        ["66,cl"]="r BUILTIN ld ID cl ARGS rd" ["66,rd"]="r BUILTIN ld ID cl ARGS rd"
        ["66,elif"]="r BUILTIN ld ID cl ARGS rd"
        ["66,else"]="r BUILTIN ld ID cl ARGS rd" ["66,or"]="r BUILTIN ld ID cl ARGS rd"
        ["66,and"]="r BUILTIN ld ID cl ARGS rd" ["66,rp"]="r BUILTIN ld ID cl ARGS rd"
        ["66,ne"]="r BUILTIN ld ID cl ARGS rd" ["66,eq"]="r BUILTIN ld ID cl ARGS rd"
        ["66,gt"]="r BUILTIN ld ID cl ARGS rd" ["66,lt"]="r BUILTIN ld ID cl ARGS rd"
        ["66,ge"]="r BUILTIN ld ID cl ARGS rd" ["66,le"]="r BUILTIN ld ID cl ARGS rd"
        ["66,strgt"]="r BUILTIN ld ID cl ARGS rd"
        ["66,strlt"]="r BUILTIN ld ID cl ARGS rd"
        ["66,streq"]="r BUILTIN ld ID cl ARGS rd"
        ["66,strne"]="r BUILTIN ld ID cl ARGS rd"
        ["66,str"]="r BUILTIN ld ID cl ARGS rd" ["66,$"]="r BUILTIN ld ID cl ARGS rd"
        ["67,ld"]="r VAR ld ID or VAR rd" ["67,cl"]="r VAR ld ID or VAR rd"
        ["67,rd"]="r VAR ld ID or VAR rd" ["67,elif"]="r VAR ld ID or VAR rd"
        ["67,else"]="r VAR ld ID or VAR rd" ["67,or"]="r VAR ld ID or VAR rd"
        ["67,and"]="r VAR ld ID or VAR rd" ["67,rp"]="r VAR ld ID or VAR rd"
        ["67,ne"]="r VAR ld ID or VAR rd" ["67,eq"]="r VAR ld ID or VAR rd"
        ["67,gt"]="r VAR ld ID or VAR rd" ["67,lt"]="r VAR ld ID or VAR rd"
        ["67,ge"]="r VAR ld ID or VAR rd" ["67,le"]="r VAR ld ID or VAR rd"
        ["67,strgt"]="r VAR ld ID or VAR rd" ["67,strlt"]="r VAR ld ID or VAR rd"
        ["67,streq"]="r VAR ld ID or VAR rd" ["67,strne"]="r VAR ld ID or VAR rd"
        ["67,str"]="r VAR ld ID or VAR rd" ["67,$"]="r VAR ld ID or VAR rd"
        ["68,ld"]="r VAR ld ID or STR rd" ["68,cl"]="r VAR ld ID or STR rd"
        ["68,rd"]="r VAR ld ID or STR rd" ["68,elif"]="r VAR ld ID or STR rd"
        ["68,else"]="r VAR ld ID or STR rd" ["68,or"]="r VAR ld ID or STR rd"
        ["68,and"]="r VAR ld ID or STR rd" ["68,rp"]="r VAR ld ID or STR rd"
        ["68,ne"]="r VAR ld ID or STR rd" ["68,eq"]="r VAR ld ID or STR rd"
        ["68,gt"]="r VAR ld ID or STR rd" ["68,lt"]="r VAR ld ID or STR rd"
        ["68,ge"]="r VAR ld ID or STR rd" ["68,le"]="r VAR ld ID or STR rd"
        ["68,strgt"]="r VAR ld ID or STR rd" ["68,strlt"]="r VAR ld ID or STR rd"
        ["68,streq"]="r VAR ld ID or STR rd" ["68,strne"]="r VAR ld ID or STR rd"
        ["68,str"]="r VAR ld ID or STR rd" ["68,$"]="r VAR ld ID or STR rd"
        ["69,rd"]="s 28" ["69,or"]="s 29" ["69,and"]="s 30"
        ["70,ld"]="r VAR ld ID and VAR rd" ["70,cl"]="r VAR ld ID and VAR rd"
        ["70,rd"]="r VAR ld ID and VAR rd" ["70,elif"]="r VAR ld ID and VAR rd"
        ["70,else"]="r VAR ld ID and VAR rd" ["70,or"]="r VAR ld ID and VAR rd"
        ["70,and"]="r VAR ld ID and VAR rd" ["70,rp"]="r VAR ld ID and VAR rd"
        ["70,ne"]="r VAR ld ID and VAR rd" ["70,eq"]="r VAR ld ID and VAR rd"
        ["70,gt"]="r VAR ld ID and VAR rd" ["70,lt"]="r VAR ld ID and VAR rd"
        ["70,ge"]="r VAR ld ID and VAR rd" ["70,le"]="r VAR ld ID and VAR rd"
        ["70,strgt"]="r VAR ld ID and VAR rd" ["70,strlt"]="r VAR ld ID and VAR rd"
        ["70,streq"]="r VAR ld ID and VAR rd" ["70,strne"]="r VAR ld ID and VAR rd"
        ["70,str"]="r VAR ld ID and VAR rd" ["70,$"]="r VAR ld ID and VAR rd"
        ["71,ld"]="r VAR ld ID and STR rd" ["71,cl"]="r VAR ld ID and STR rd"
        ["71,rd"]="r VAR ld ID and STR rd" ["71,elif"]="r VAR ld ID and STR rd"
        ["71,else"]="r VAR ld ID and STR rd" ["71,or"]="r VAR ld ID and STR rd"
        ["71,and"]="r VAR ld ID and STR rd" ["71,rp"]="r VAR ld ID and STR rd"
        ["71,ne"]="r VAR ld ID and STR rd" ["71,eq"]="r VAR ld ID and STR rd"
        ["71,gt"]="r VAR ld ID and STR rd" ["71,lt"]="r VAR ld ID and STR rd"
        ["71,ge"]="r VAR ld ID and STR rd" ["71,le"]="r VAR ld ID and STR rd"
        ["71,strgt"]="r VAR ld ID and STR rd" ["71,strlt"]="r VAR ld ID and STR rd"
        ["71,streq"]="r VAR ld ID and STR rd" ["71,strne"]="r VAR ld ID and STR rd"
        ["71,str"]="r VAR ld ID and STR rd" ["71,$"]="r VAR ld ID and STR rd"
        ["72,rd"]="r ELSE" ["72,elif"]="s 79" ["72,else"]="s 80" ["72,ELSE"]="78"
        ["73,cl"]="r BOOLO BOOLO or UOP BOOLA" ["73,or"]="r BOOLO BOOLO or UOP BOOLA"
        ["73,and"]="s 34" ["73,rp"]="r BOOLO BOOLO or UOP BOOLA"
        ["74,cl"]="r BOOLA BOOLA and UOP BOOL" ["74,or"]="r BOOLA BOOLA and UOP BOOL"
        ["74,and"]="r BOOLA BOOLA and UOP BOOL" ["74,rp"]="r BOOLA BOOLA and UOP BOOL"
        ["75,ld"]="s 9" ["75,lp"]="s 21" ["75,ex"]="s 20" ["75,str"]="s 10"
        ["75,STMT"]="24" ["75,IF"]="3" ["75,FORIN"]="4" ["75,INCLUDE"]="5"
        ["75,BUILTIN"]="6" ["75,VAR"]="7" ["75,STR"]="8" ["75,BOOLS"]="81"
        ["75,BOOLO"]="17" ["75,UOP"]="18" ["75,BOOLA"]="19" ["75,BOOL"]="22"
        ["75,ARGS"]="23" ["76,rp"]="s 82" ["77,ld"]="r DOC" ["77,rd"]="r DOC"
        ["77,str"]="r DOC" ["77,DOC"]="83" ["78,rd"]="s 84" ["79,ld"]="s 9"
        ["79,lp"]="s 21" ["79,ex"]="s 20" ["79,str"]="s 10" ["79,STMT"]="24"
        ["79,IF"]="3" ["79,FORIN"]="4" ["79,INCLUDE"]="5" ["79,BUILTIN"]="6"
        ["79,VAR"]="7" ["79,STR"]="8" ["79,BOOLS"]="85" ["79,BOOLO"]="17"
        ["79,UOP"]="18" ["79,BOOLA"]="19" ["79,BOOL"]="22" ["79,ARGS"]="23"
        ["80,cl"]="s 86" ["81,rp"]="s 87" ["82,cl"]="r BOOLA BOOLA and lp BOOLS rp"
        ["82,or"]="r BOOLA BOOLA and lp BOOLS rp"
        ["82,and"]="r BOOLA BOOLA and lp BOOLS rp"
        ["82,rp"]="r BOOLA BOOLA and lp BOOLS rp" ["83,ld"]="s 9" ["83,rd"]="s 88"
        ["83,str"]="s 10" ["83,STMT"]="2" ["83,IF"]="3" ["83,FORIN"]="4"
        ["83,INCLUDE"]="5" ["83,BUILTIN"]="6" ["83,VAR"]="7" ["83,STR"]="8"
        ["84,ld"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,cl"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,rd"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,elif"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,else"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,or"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,and"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,rp"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,ne"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,eq"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,gt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,lt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,ge"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,le"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,strgt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,strlt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,streq"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,strne"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,str"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
        ["84,$"]="r IF ld if BOOLS cl DOC ELIF ELSE rd" ["85,cl"]="s 89"
        ["86,ld"]="r DOC" ["86,rd"]="r DOC" ["86,str"]="r DOC" ["86,DOC"]="90"
        ["87,cl"]="r BOOLA BOOLA and UOP lp BOOLS rp"
        ["87,or"]="r BOOLA BOOLA and UOP lp BOOLS rp"
        ["87,and"]="r BOOLA BOOLA and UOP lp BOOLS rp"
        ["87,rp"]="r BOOLA BOOLA and UOP lp BOOLS rp"
        ["88,ld"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,cl"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,rd"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,elif"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,else"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,or"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,and"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,rp"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,ne"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,eq"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,gt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,lt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,ge"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,le"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,strgt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,strlt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,streq"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,strne"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,str"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["88,$"]="r FORIN ld for ID in ARGS cl DOC rd" ["89,ld"]="r DOC"
        ["89,rd"]="r DOC" ["89,elif"]="r DOC" ["89,else"]="r DOC" ["89,str"]="r DOC"
        ["89,DOC"]="91" ["90,ld"]="s 9" ["90,rd"]="r ELSE else cl DOC" ["90,str"]="s 10"
        ["90,STMT"]="2" ["90,IF"]="3" ["90,FORIN"]="4" ["90,INCLUDE"]="5"
        ["90,BUILTIN"]="6" ["90,VAR"]="7" ["90,STR"]="8" ["91,ld"]="s 9"
        ["91,rd"]="r ELIF ELIF elif BOOLS cl DOC"
        ["91,elif"]="r ELIF ELIF elif BOOLS cl DOC"
        ["91,else"]="r ELIF ELIF elif BOOLS cl DOC" ["91,str"]="s 10" ["91,STMT"]="2"
        ["91,IF"]="3" ["91,FORIN"]="4" ["91,INCLUDE"]="5" ["91,BUILTIN"]="6"
        ["91,VAR"]="7" ["91,STR"]="8"
    ) # }}}
    # <<< BPT_PARSE_TABLE_E <<<

    # Deduplication function for collect-{var,include}
    bpt.__dedup() { echo -n "$1" | sort | uniq; }

    # Append this if reducer is bpt.__reduce_generate
    local HEADER=''
    [[ $reduce_fn != bpt.__reduce_generate ]] || {
        read -r -d '' HEADER <<-'EOF'
#!/bin/bash
e(){ local OIFS="$IFS"; IFS=; echo -n "$*"; IFS="$OIFS"; };
len(){ echo -n "${#1}"; };
seq(){ command seq -s ' ' -- "$@" || kill $__BPT_BASHPID; };
split(){ echo -n "$*"; };
EOF
        HEADER+=$'\n'
    }

    # Execute command
    case "$cmd" in
    scan) bpt.scan "$ld" "$rd" <"$infile" ;;
    fingerprint) bpt.fingerprint "$ld" "$rd" "$infile" "$debug" ;;
    *) result="$(bpt.process "$ld" "$rd" "$reduce_fn" "$infile" "$debug")" &&
        (__BPT_BASHPID=$BASHPID && $post_process "$HEADER$result" || return $?) ;;
    esac
)

return 0 2>/dev/null || bpt.main "$@"
