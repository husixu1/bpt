#!/bin/bash
# vim: set foldlevel=0 foldmethod=marker:
# shellcheck disable=SC2317

# Import once
[[ -z $__BPT_VERSION ]] || return
readonly __BPT_VERSION="v0.2"

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
    local -r error_fn="${3:-bpt.__error}"
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
    bpt.__shift() {
        states+=":$1"
        contents["$stack_size"]="$content"
        ((++stack_size))
        token='' content=''
    }

    # $1: Rule
    bpt.__reduce() {
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
    bpt.__accept() {
        printf '%s' "${contents[1]}"
    }

    # Default error handler
    bpt.__error() {
        echo "Error: Line $(($1 + 1)) Column $(($2 + 1))"
        echo "$3"
    } >&2

    # Debugging support
    $NDEBUG || {
        eval __orig_"$(declare -f bpt.__shift)"
        eval __orig_"$(declare -f bpt.__reduce)"
        eval __orig_"$(declare -f bpt.__accept)"
        bpt.__shift() {
            echo "[DBG] ${states##*:} Shift $1 \`$content\`" >&2
            __orig_bpt.__shift "$@"
        }
        bpt.__reduce() {
            echo "[DBG] ${states##*:} Reduce ${rule[*]}" >&2
            __orig_bpt.__reduce
        }
        bpt.__accept() {
            $NDEBUG || echo "[DBG] Result accepted" >&2
            __orig_bpt.__accept
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
        s*) bpt.__shift "${action#s }" ;;
        # Reduce
        r*) # shellcheck disable=SC2206
            rule=(${action#r })
            bpt.__reduce
            ;;
        # Accept
        a) bpt.__accept && break ;;
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
                "Expecting \`${expects[*]}\` but got \`${token}\` ($content)."
            $NDEBUG || echo "[DBG] PARSER STATES ${states} TOKEN ${token} CONTENT ${content}." >&2
            exit 1
            ;;
        *) # Parse table error (internal error)
            echo "Internal error: STATES ${states} TOKEN ${token} CONTENT ${content}. " >&2
            echo "Internal error: Action '$action' not recognized." >&2
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
#   strcm: =~
#   and|or|if|elif|else|for|in|include|quote: <as is>
#   id: [[:alpha:]_][[:alnum:]_]*
#
# shellcheck disable=SC2030
bpt.scan() (
    local -r ld="$1" rd="$2" error_fn="${3:-bpt.__scan_error}"
    bpt.__test_delims "$ld" "$rd" || return 1

    # Default error handler
    bpt.__scan_error() {
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

    local -a KW=('==' '!=' '=~' '>' '<' ':' '\!' '"' "'" '\(' '\)') # Keywords
    local -a SKW=( # Keywords ending with an alphanumeric character
        '-eq' '-ne' '-gt' '-lt' '-ge' '-le'
        'and' 'or' 'if' 'elif' 'else' 'for' 'in' 'include' 'quote'
    )
    if [[ "$e_ld" =~ [[:alnum:]_]$ ]]; then SKW+=("$e_ld"); else KW+=("$e_ld"); fi
    if [[ "$e_rd" =~ [[:alnum:]_]$ ]]; then SKW+=("$e_rd"); else KW+=("$e_rd"); fi
    local -r KW_RE="$(IFS='|' && echo -n "${KW[*]}")"
    local -r SKW_RE="$(IFS='|' && echo -n "${SKW[*]}")"
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
    bpt.__start_string() {
        str_lines="${str_lines:-1}"
        str_bytes="${str_bytes:-$num_bytes}"
    }
    # Commit (possibly multiline) string buffer
    # shellcheck disable=SC2031
    bpt.__commit_string() {
        ((str_lines > 0)) || return
        echo "str $str_lines $((num_lines + 1 - str_lines)) $str_bytes"
        # `$content` can be a literal `-ne`. Thus printf is needed.
        printf '%s\n' "$string"
        string='' str_lines='' str_bytes=''
    }

    # Tokenizer
    local line='' content='' newline=true
    bpt.__start_string
    while IFS= read -r line || { newline=false && false; } || [[ $line ]]; do
        # Scan the line
        while [[ -n "$line" ]]; do
            content='' # The consumed content (to be removed from `line`)
            if [[ $num_ld -eq 0 ]]; then
                # Outside `ld ... rd`
                if [[ $line =~ ^(${e_ld}) ]]; then
                    # If met `ld`, enter `ld ... rd`
                    bpt.__commit_string
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
                    bpt.__commit_string
                else
                    content="$line_copy"
                    string+="$line_copy"
                fi
            else
                # Non-strings. Commit string first.
                bpt.__commit_string
                if [[ $line =~ ^(${KW_RE}) ||
                    $line =~ ^(${SKW_RE})($|[^[:alnum:]_]) ]]; then
                    # Inside `ld ... rd` and matches a keyword at front
                    content="${BASH_REMATCH[1]}"
                    case "$content" in
                    '-eq') echo -n eq ;; '-ne') echo -n ne ;;
                    '-lt') echo -n lt ;; '-gt') echo -n gt ;;
                    '-le') echo -n le ;; '-ge') echo -n ge ;;
                    '==') echo -n streq ;; '!=') echo -n strne ;;
                    '>') echo -n strgt ;; '<') echo -n strlt ;;
                    '=~') echo -n strcm ;;
                    '!') echo -n ex ;; ':') echo -n cl ;;
                    '(') echo -n lp ;; ')') echo -n rp ;;
                    and | or | if | elif | else) ;&
                    for | in | include | quote) echo -n "$content" ;;
                    '"' | "'")
                        quote="$content"
                        bpt.__start_string
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
                        ((num_ld != 0)) || bpt.__start_string
                        echo -n rd
                        ;;
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
                    [[ ! $line =~ ^([[:space:]]+)(.*) ]] || {
                        line="${BASH_REMATCH[2]}"
                        ((num_bytes += ${#BASH_REMATCH[1]}))
                        continue
                    }
                    content="$line"

                    # Strip possible keywords suffixing variable names
                    ! [[ $content =~ (${KW_RE}) ||
                        $content =~ (${SKW_RE})($|[^[:alnum:]_]?) ]] ||
                        content="${content%%"${BASH_REMATCH[1]}"*}"

                    # Contents must be keywords
                    [[ $content =~ ^(${ID_RE}) ]] || {
                        $error_fn "$num_lines" "$num_bytes" \
                            "'$content' is not a valid identifier"
                        return 1
                    }
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
            bpt.__start_string
            ! $newline || { string+=$'\n' && ((++str_lines)); }
        }

        newline=true
    done
    bpt.__commit_string
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
    BUILTIN | QUOTE) contents[$s]="${contents[$((s + 3))]}" ;;
    INCLUDE) contents[$s]="$(bpt.__recursive_process "${contents[$((s + 3))]}")"$'\n' ;;
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
        contents[$s]+="$(bpt.__recursive_process "${contents[$((s + 3))]}")"
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
    VAR) case "${rule[3]}" in
        rd) contents[$s]="\${${contents[$((s + 1))]%:*:*}}" ;;
        or) contents[$s]="\${${contents[$((s + 1))]%:*:*}:-\$(e " ;;&
        and) contents[$s]="\${${contents[$((s + 1))]%:*:*}:+\$(e " ;;&
        *) case "${rule[4]}" in
            VAR) contents[$s]+="\"${contents[$((s + 3))]}\")}" ;;
            STR) contents[$s]+="${contents[$((s + 3))]@Q})}" ;;
            esac ;;
        esac ;;
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
        VAR | BUILTIN | QUOTE) contents[$s]+=" $stmt " ;;
        INCLUDE | FORIN | IF) contents[$s]+=" \"\$($stmt)\" " ;;
        esac
        ;;
    QUOTE) contents[$s]="\"\$(e ${contents[$((s + 3))]})\"" ;;
    BUILTIN) # Filter allowed builtints
        local builtin_name=${contents[$((s + 1))]%:*:*}
        case "$builtin_name" in
        len | seq | split) contents[$s]="\$($builtin_name ${contents[$((s + 3))]})" ;;
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
    INCLUDE) contents[$s]="$(bpt.__recursive_process "${contents[$((s + 3))]}")" ;;
    FORIN) contents[$s]="for ${contents[$((s + 2))]%:*:*} in ${contents[$((s + 4))]}; do ${contents[$((s + 6))]} done" ;;
    BOOL) case "${#rule[@]}" in
        2) contents[$s]="\"\$(e ${contents[$s]})\"" ;;
        4) case "${contents[$((s + 1))]}" in # Don't quote the rhs of `=~`
            '=~') contents[$s]+=" ${contents[$((s + 1))]} \$(e ${contents[$((s + 2))]})" ;;
            *) contents[$s]+=" ${contents[$((s + 1))]} \"\$(e ${contents[$((s + 2))]})\"" ;;
            esac ;;
        esac ;;
    BOOLA) case "${#rule[@]}" in
        4) case "${rule[1]}" in
            BOOLA) contents[$s]="${contents[$s]} && ${contents[$((s + 2))]}" ;;
            lp) contents[$s]="( ${contents[$((s + 1))]} )" ;;
            esac ;;
        5) contents[$s]="${contents[$s]} && ${contents[$((s + 2))]} ${contents[$((s + 3))]}" ;;
        6) contents[$s]="${contents[$s]} && ( ${contents[$((s + 3))]} )" ;;
        7) contents[$s]="${contents[$s]} && ${contents[$((s + 2))]} ( ${contents[$((s + 4))]} )" ;;
        esac ;;
    BOOLO) case "${#rule[@]}" in
        4) contents[$s]="${contents[$s]} || ${contents[$((s + 2))]}" ;;
        5) contents[$s]="${contents[$s]} || ${contents[$((s + 2))]} ${contents[$((s + 3))]}" ;;
        esac ;;
    BOOLS) [[ ${#rule[@]} -eq 2 ]] || contents[$s]="${contents[$s]} ${contents[$((s + 1))]}" ;;
    ELSE) case "${#rule[@]}" in
        1) contents[$s]='' ;;
        *) contents[$s]="else ${contents[$((s + 2))]}" ;;
        esac ;;
    ELIF) case "${#rule[@]}" in
        1) contents[$s]='' ;;
        *) contents[$s]="${contents[$s]} elif [[ ${contents[$((s + 2))]} ]]; then ${contents[$((s + 4))]}" ;;
        esac ;;
    IF) case "${#rule[@]}" in
        9) contents[$s]="if [[ ${contents[$((s + 2))]} ]]; then ${contents[$((s + 4))]}${contents[$((s + 5))]}${contents[$((s + 6))]} fi" ;;
        8) contents[$s]="if [[ ${contents[$((s + 1))]} ]]; then ${contents[$((s + 3))]} else ${contents[$((s + 5))]} fi" ;;
        6) contents[$s]="if [[ ${contents[$((s + 1))]} ]]; then ${contents[$((s + 3))]} fi" ;;
        4) contents[$s]="if [[ ${contents[$((s + 1))]} ]]; then { e true; }; else { e false; }; fi" ;;
        esac ;;
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
        QUOTE) contents[$s]+="{ e $stmt; };" ;;
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
#   STMT    -> IF | FORIN | INCLUDE | BUILTIN | QUOTE | VAR | STR .
#   IF      -> ld if BOOLS cl DOC ELIF ELSE rd
#            | ld BOOLS cl DOC cl DOC rd
#            | ld BOOLS cl DOC rd
#            | ld BOOLS rd .
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
#   QUOTE   -> ld quote cl ARGS rd .
#   ARGS    -> ARGS STMT
#            | STMT .
#   VAR     -> ld ID rd
#            | ld ID or VAR rd
#            | ld ID or STR rd
#            | ld ID and VAR rd
#            | ld ID and STR rd .
#   BOP     -> ne     | eq    | gt    | lt    | ge    | le
#            | strne  | streq | strgt | strlt | strcm .
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
    bpt.__recursive_process() {
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
    bpt.__error_handler() {
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
    bpt.__lr_parse __BPT_PARSE_TABLE "$reduce_fn" bpt.__error_handler "$debug" \
        < <(bpt.scan "$ld" "$rd" bpt.__error_handler <"$file")
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
        [[ ${!var+.} ]] || {
            echo "Error: variable '$var' is required but not set" >&2
            return 1
        }
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
    echo '    {{ {{x}}: {{var1}} : {{var2}} }}'
    echo '    {{ if {{x}}: {{var1}} else : {{var2}} }}'
    echo '    {{ if {{x}} -gt "5": {{var1}} elif: {{var2}} else: {{var3}} }}'
    echo '    {{ if ({{var1}} > "abc" and {{var2}} < "def") or {{var3}} == "hello" : {{ include : "input2.tpl" }} }}'
    echo
    echo "    Available operators are: "
    echo "      compare numbers:   -ne, -eq, -gt, -lt, -ge, -le "
    echo "      compare strings:   >, <, ==, !=, =~"
    echo "      logical operators: and, or, !"
    echo "      grouping:          ()"
    echo
    echo "  Iterate a list"
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

    # Deduplication function for collect-{var,include}
    bpt.__dedup() { echo -n "$1" | sort | uniq; }

    # Append this if reducer is bpt.__reduce_generate
    local HEADER=''
    [[ $reduce_fn != bpt.__reduce_generate ]] || {
        read -r -d '' HEADER <<-'EOF'
#!/bin/bash
\unset -f echo || true
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

# shellcheck disable=SC2034
# >>> BPT_PARSE_TABLE_S >>>
readonly -A __BPT_PARSE_TABLE=( # {{{
    ["0,ld"]="r DOC" ["0,str"]="r DOC" ["0,$"]="r DOC" ["0,DOC"]="1" ["1,ld"]="s 10"
    ["1,str"]="s 11" ["1,$"]="a" ["1,STMT"]="2" ["1,IF"]="3" ["1,FORIN"]="4"
    ["1,INCLUDE"]="5" ["1,BUILTIN"]="6" ["1,QUOTE"]="7" ["1,VAR"]="8" ["1,STR"]="9"
    ["2,ld"]="r DOC DOC STMT" ["2,cl"]="r DOC DOC STMT" ["2,rd"]="r DOC DOC STMT"
    ["2,elif"]="r DOC DOC STMT" ["2,else"]="r DOC DOC STMT"
    ["2,str"]="r DOC DOC STMT" ["2,$"]="r DOC DOC STMT" ["3,ld"]="r STMT IF"
    ["3,cl"]="r STMT IF" ["3,rd"]="r STMT IF" ["3,elif"]="r STMT IF"
    ["3,else"]="r STMT IF" ["3,or"]="r STMT IF" ["3,and"]="r STMT IF"
    ["3,rp"]="r STMT IF" ["3,ne"]="r STMT IF" ["3,eq"]="r STMT IF"
    ["3,gt"]="r STMT IF" ["3,lt"]="r STMT IF" ["3,ge"]="r STMT IF"
    ["3,le"]="r STMT IF" ["3,strne"]="r STMT IF" ["3,streq"]="r STMT IF"
    ["3,strgt"]="r STMT IF" ["3,strlt"]="r STMT IF" ["3,strcm"]="r STMT IF"
    ["3,str"]="r STMT IF" ["3,$"]="r STMT IF" ["4,ld"]="r STMT FORIN"
    ["4,cl"]="r STMT FORIN" ["4,rd"]="r STMT FORIN" ["4,elif"]="r STMT FORIN"
    ["4,else"]="r STMT FORIN" ["4,or"]="r STMT FORIN" ["4,and"]="r STMT FORIN"
    ["4,rp"]="r STMT FORIN" ["4,ne"]="r STMT FORIN" ["4,eq"]="r STMT FORIN"
    ["4,gt"]="r STMT FORIN" ["4,lt"]="r STMT FORIN" ["4,ge"]="r STMT FORIN"
    ["4,le"]="r STMT FORIN" ["4,strne"]="r STMT FORIN" ["4,streq"]="r STMT FORIN"
    ["4,strgt"]="r STMT FORIN" ["4,strlt"]="r STMT FORIN" ["4,strcm"]="r STMT FORIN"
    ["4,str"]="r STMT FORIN" ["4,$"]="r STMT FORIN" ["5,ld"]="r STMT INCLUDE"
    ["5,cl"]="r STMT INCLUDE" ["5,rd"]="r STMT INCLUDE" ["5,elif"]="r STMT INCLUDE"
    ["5,else"]="r STMT INCLUDE" ["5,or"]="r STMT INCLUDE" ["5,and"]="r STMT INCLUDE"
    ["5,rp"]="r STMT INCLUDE" ["5,ne"]="r STMT INCLUDE" ["5,eq"]="r STMT INCLUDE"
    ["5,gt"]="r STMT INCLUDE" ["5,lt"]="r STMT INCLUDE" ["5,ge"]="r STMT INCLUDE"
    ["5,le"]="r STMT INCLUDE" ["5,strne"]="r STMT INCLUDE"
    ["5,streq"]="r STMT INCLUDE" ["5,strgt"]="r STMT INCLUDE"
    ["5,strlt"]="r STMT INCLUDE" ["5,strcm"]="r STMT INCLUDE"
    ["5,str"]="r STMT INCLUDE" ["5,$"]="r STMT INCLUDE" ["6,ld"]="r STMT BUILTIN"
    ["6,cl"]="r STMT BUILTIN" ["6,rd"]="r STMT BUILTIN" ["6,elif"]="r STMT BUILTIN"
    ["6,else"]="r STMT BUILTIN" ["6,or"]="r STMT BUILTIN" ["6,and"]="r STMT BUILTIN"
    ["6,rp"]="r STMT BUILTIN" ["6,ne"]="r STMT BUILTIN" ["6,eq"]="r STMT BUILTIN"
    ["6,gt"]="r STMT BUILTIN" ["6,lt"]="r STMT BUILTIN" ["6,ge"]="r STMT BUILTIN"
    ["6,le"]="r STMT BUILTIN" ["6,strne"]="r STMT BUILTIN"
    ["6,streq"]="r STMT BUILTIN" ["6,strgt"]="r STMT BUILTIN"
    ["6,strlt"]="r STMT BUILTIN" ["6,strcm"]="r STMT BUILTIN"
    ["6,str"]="r STMT BUILTIN" ["6,$"]="r STMT BUILTIN" ["7,ld"]="r STMT QUOTE"
    ["7,cl"]="r STMT QUOTE" ["7,rd"]="r STMT QUOTE" ["7,elif"]="r STMT QUOTE"
    ["7,else"]="r STMT QUOTE" ["7,or"]="r STMT QUOTE" ["7,and"]="r STMT QUOTE"
    ["7,rp"]="r STMT QUOTE" ["7,ne"]="r STMT QUOTE" ["7,eq"]="r STMT QUOTE"
    ["7,gt"]="r STMT QUOTE" ["7,lt"]="r STMT QUOTE" ["7,ge"]="r STMT QUOTE"
    ["7,le"]="r STMT QUOTE" ["7,strne"]="r STMT QUOTE" ["7,streq"]="r STMT QUOTE"
    ["7,strgt"]="r STMT QUOTE" ["7,strlt"]="r STMT QUOTE" ["7,strcm"]="r STMT QUOTE"
    ["7,str"]="r STMT QUOTE" ["7,$"]="r STMT QUOTE" ["8,ld"]="r STMT VAR"
    ["8,cl"]="r STMT VAR" ["8,rd"]="r STMT VAR" ["8,elif"]="r STMT VAR"
    ["8,else"]="r STMT VAR" ["8,or"]="r STMT VAR" ["8,and"]="r STMT VAR"
    ["8,rp"]="r STMT VAR" ["8,ne"]="r STMT VAR" ["8,eq"]="r STMT VAR"
    ["8,gt"]="r STMT VAR" ["8,lt"]="r STMT VAR" ["8,ge"]="r STMT VAR"
    ["8,le"]="r STMT VAR" ["8,strne"]="r STMT VAR" ["8,streq"]="r STMT VAR"
    ["8,strgt"]="r STMT VAR" ["8,strlt"]="r STMT VAR" ["8,strcm"]="r STMT VAR"
    ["8,str"]="r STMT VAR" ["8,$"]="r STMT VAR" ["9,ld"]="r STMT STR"
    ["9,cl"]="r STMT STR" ["9,rd"]="r STMT STR" ["9,elif"]="r STMT STR"
    ["9,else"]="r STMT STR" ["9,or"]="r STMT STR" ["9,and"]="r STMT STR"
    ["9,rp"]="r STMT STR" ["9,ne"]="r STMT STR" ["9,eq"]="r STMT STR"
    ["9,gt"]="r STMT STR" ["9,lt"]="r STMT STR" ["9,ge"]="r STMT STR"
    ["9,le"]="r STMT STR" ["9,strne"]="r STMT STR" ["9,streq"]="r STMT STR"
    ["9,strgt"]="r STMT STR" ["9,strlt"]="r STMT STR" ["9,strcm"]="r STMT STR"
    ["9,str"]="r STMT STR" ["9,$"]="r STMT STR" ["10,ld"]="s 10" ["10,if"]="s 12"
    ["10,lp"]="s 23" ["10,for"]="s 14" ["10,include"]="s 15" ["10,quote"]="s 17"
    ["10,ex"]="s 22" ["10,id"]="s 20" ["10,str"]="s 11" ["10,STMT"]="26"
    ["10,IF"]="3" ["10,FORIN"]="4" ["10,INCLUDE"]="5" ["10,BUILTIN"]="6"
    ["10,QUOTE"]="7" ["10,VAR"]="8" ["10,STR"]="9" ["10,BOOLS"]="13"
    ["10,BOOLO"]="18" ["10,UOP"]="19" ["10,BOOLA"]="21" ["10,BOOL"]="24"
    ["10,ARGS"]="25" ["10,ID"]="16" ["11,ld"]="r STR str" ["11,cl"]="r STR str"
    ["11,rd"]="r STR str" ["11,elif"]="r STR str" ["11,else"]="r STR str"
    ["11,or"]="r STR str" ["11,and"]="r STR str" ["11,rp"]="r STR str"
    ["11,ne"]="r STR str" ["11,eq"]="r STR str" ["11,gt"]="r STR str"
    ["11,lt"]="r STR str" ["11,ge"]="r STR str" ["11,le"]="r STR str"
    ["11,strne"]="r STR str" ["11,streq"]="r STR str" ["11,strgt"]="r STR str"
    ["11,strlt"]="r STR str" ["11,strcm"]="r STR str" ["11,str"]="r STR str"
    ["11,$"]="r STR str" ["12,ld"]="s 10" ["12,lp"]="s 23" ["12,ex"]="s 22"
    ["12,str"]="s 11" ["12,STMT"]="26" ["12,IF"]="3" ["12,FORIN"]="4"
    ["12,INCLUDE"]="5" ["12,BUILTIN"]="6" ["12,QUOTE"]="7" ["12,VAR"]="8"
    ["12,STR"]="9" ["12,BOOLS"]="27" ["12,BOOLO"]="18" ["12,UOP"]="19"
    ["12,BOOLA"]="21" ["12,BOOL"]="24" ["12,ARGS"]="25" ["13,cl"]="s 28"
    ["13,rd"]="s 29" ["14,id"]="s 20" ["14,ID"]="30" ["15,cl"]="s 31"
    ["16,cl"]="s 32" ["16,rd"]="s 33" ["16,or"]="s 34" ["16,and"]="s 35"
    ["17,cl"]="s 36" ["18,cl"]="r BOOLS BOOLO" ["18,rd"]="r BOOLS BOOLO"
    ["18,or"]="s 37" ["18,rp"]="r BOOLS BOOLO" ["19,ld"]="s 10" ["19,lp"]="s 23"
    ["19,ex"]="s 22" ["19,str"]="s 11" ["19,STMT"]="26" ["19,IF"]="3"
    ["19,FORIN"]="4" ["19,INCLUDE"]="5" ["19,BUILTIN"]="6" ["19,QUOTE"]="7"
    ["19,VAR"]="8" ["19,STR"]="9" ["19,BOOLS"]="38" ["19,BOOLO"]="18"
    ["19,UOP"]="19" ["19,BOOLA"]="21" ["19,BOOL"]="24" ["19,ARGS"]="25"
    ["20,cl"]="r ID id" ["20,rd"]="r ID id" ["20,or"]="r ID id" ["20,and"]="r ID id"
    ["20,in"]="r ID id" ["21,cl"]="r BOOLO BOOLA" ["21,rd"]="r BOOLO BOOLA"
    ["21,or"]="r BOOLO BOOLA" ["21,and"]="s 39" ["21,rp"]="r BOOLO BOOLA"
    ["22,ld"]="r UOP ex" ["22,lp"]="r UOP ex" ["22,ex"]="r UOP ex"
    ["22,str"]="r UOP ex" ["23,ld"]="s 10" ["23,lp"]="s 23" ["23,ex"]="s 22"
    ["23,str"]="s 11" ["23,STMT"]="26" ["23,IF"]="3" ["23,FORIN"]="4"
    ["23,INCLUDE"]="5" ["23,BUILTIN"]="6" ["23,QUOTE"]="7" ["23,VAR"]="8"
    ["23,STR"]="9" ["23,BOOLS"]="40" ["23,BOOLO"]="18" ["23,UOP"]="19"
    ["23,BOOLA"]="21" ["23,BOOL"]="24" ["23,ARGS"]="25" ["24,cl"]="r BOOLA BOOL"
    ["24,rd"]="r BOOLA BOOL" ["24,or"]="r BOOLA BOOL" ["24,and"]="r BOOLA BOOL"
    ["24,rp"]="r BOOLA BOOL" ["25,ld"]="s 10" ["25,cl"]="r BOOL ARGS"
    ["25,rd"]="r BOOL ARGS" ["25,or"]="r BOOL ARGS" ["25,and"]="r BOOL ARGS"
    ["25,rp"]="r BOOL ARGS" ["25,ne"]="s 43" ["25,eq"]="s 44" ["25,gt"]="s 45"
    ["25,lt"]="s 46" ["25,ge"]="s 47" ["25,le"]="s 48" ["25,strne"]="s 49"
    ["25,streq"]="s 50" ["25,strgt"]="s 51" ["25,strlt"]="s 52" ["25,strcm"]="s 53"
    ["25,str"]="s 11" ["25,STMT"]="42" ["25,IF"]="3" ["25,FORIN"]="4"
    ["25,INCLUDE"]="5" ["25,BUILTIN"]="6" ["25,QUOTE"]="7" ["25,VAR"]="8"
    ["25,STR"]="9" ["25,BOP"]="41" ["26,ld"]="r ARGS STMT" ["26,cl"]="r ARGS STMT"
    ["26,rd"]="r ARGS STMT" ["26,or"]="r ARGS STMT" ["26,and"]="r ARGS STMT"
    ["26,rp"]="r ARGS STMT" ["26,ne"]="r ARGS STMT" ["26,eq"]="r ARGS STMT"
    ["26,gt"]="r ARGS STMT" ["26,lt"]="r ARGS STMT" ["26,ge"]="r ARGS STMT"
    ["26,le"]="r ARGS STMT" ["26,strne"]="r ARGS STMT" ["26,streq"]="r ARGS STMT"
    ["26,strgt"]="r ARGS STMT" ["26,strlt"]="r ARGS STMT" ["26,strcm"]="r ARGS STMT"
    ["26,str"]="r ARGS STMT" ["27,cl"]="s 54" ["28,ld"]="r DOC" ["28,cl"]="r DOC"
    ["28,rd"]="r DOC" ["28,str"]="r DOC" ["28,DOC"]="55"
    ["29,ld"]="r IF ld BOOLS rd" ["29,cl"]="r IF ld BOOLS rd"
    ["29,rd"]="r IF ld BOOLS rd" ["29,elif"]="r IF ld BOOLS rd"
    ["29,else"]="r IF ld BOOLS rd" ["29,or"]="r IF ld BOOLS rd"
    ["29,and"]="r IF ld BOOLS rd" ["29,rp"]="r IF ld BOOLS rd"
    ["29,ne"]="r IF ld BOOLS rd" ["29,eq"]="r IF ld BOOLS rd"
    ["29,gt"]="r IF ld BOOLS rd" ["29,lt"]="r IF ld BOOLS rd"
    ["29,ge"]="r IF ld BOOLS rd" ["29,le"]="r IF ld BOOLS rd"
    ["29,strne"]="r IF ld BOOLS rd" ["29,streq"]="r IF ld BOOLS rd"
    ["29,strgt"]="r IF ld BOOLS rd" ["29,strlt"]="r IF ld BOOLS rd"
    ["29,strcm"]="r IF ld BOOLS rd" ["29,str"]="r IF ld BOOLS rd"
    ["29,$"]="r IF ld BOOLS rd" ["30,in"]="s 56" ["31,str"]="s 11" ["31,STR"]="57"
    ["32,ld"]="s 10" ["32,str"]="s 11" ["32,STMT"]="26" ["32,IF"]="3"
    ["32,FORIN"]="4" ["32,INCLUDE"]="5" ["32,BUILTIN"]="6" ["32,QUOTE"]="7"
    ["32,VAR"]="8" ["32,STR"]="9" ["32,ARGS"]="58" ["33,ld"]="r VAR ld ID rd"
    ["33,cl"]="r VAR ld ID rd" ["33,rd"]="r VAR ld ID rd"
    ["33,elif"]="r VAR ld ID rd" ["33,else"]="r VAR ld ID rd"
    ["33,or"]="r VAR ld ID rd" ["33,and"]="r VAR ld ID rd"
    ["33,rp"]="r VAR ld ID rd" ["33,ne"]="r VAR ld ID rd" ["33,eq"]="r VAR ld ID rd"
    ["33,gt"]="r VAR ld ID rd" ["33,lt"]="r VAR ld ID rd" ["33,ge"]="r VAR ld ID rd"
    ["33,le"]="r VAR ld ID rd" ["33,strne"]="r VAR ld ID rd"
    ["33,streq"]="r VAR ld ID rd" ["33,strgt"]="r VAR ld ID rd"
    ["33,strlt"]="r VAR ld ID rd" ["33,strcm"]="r VAR ld ID rd"
    ["33,str"]="r VAR ld ID rd" ["33,$"]="r VAR ld ID rd" ["34,ld"]="s 61"
    ["34,str"]="s 11" ["34,VAR"]="59" ["34,STR"]="60" ["35,ld"]="s 61"
    ["35,str"]="s 11" ["35,VAR"]="62" ["35,STR"]="63" ["36,ld"]="s 10"
    ["36,str"]="s 11" ["36,STMT"]="26" ["36,IF"]="3" ["36,FORIN"]="4"
    ["36,INCLUDE"]="5" ["36,BUILTIN"]="6" ["36,QUOTE"]="7" ["36,VAR"]="8"
    ["36,STR"]="9" ["36,ARGS"]="64" ["37,ld"]="s 10" ["37,lp"]="s 23"
    ["37,ex"]="s 22" ["37,str"]="s 11" ["37,STMT"]="26" ["37,IF"]="3"
    ["37,FORIN"]="4" ["37,INCLUDE"]="5" ["37,BUILTIN"]="6" ["37,QUOTE"]="7"
    ["37,VAR"]="8" ["37,STR"]="9" ["37,UOP"]="66" ["37,BOOLA"]="65" ["37,BOOL"]="24"
    ["37,ARGS"]="25" ["38,cl"]="r BOOLS UOP BOOLS" ["38,rd"]="r BOOLS UOP BOOLS"
    ["38,rp"]="r BOOLS UOP BOOLS" ["39,ld"]="s 10" ["39,lp"]="s 69" ["39,ex"]="s 22"
    ["39,str"]="s 11" ["39,STMT"]="26" ["39,IF"]="3" ["39,FORIN"]="4"
    ["39,INCLUDE"]="5" ["39,BUILTIN"]="6" ["39,QUOTE"]="7" ["39,VAR"]="8"
    ["39,STR"]="9" ["39,UOP"]="68" ["39,BOOL"]="67" ["39,ARGS"]="25"
    ["40,rp"]="s 70" ["41,ld"]="s 10" ["41,str"]="s 11" ["41,STMT"]="26"
    ["41,IF"]="3" ["41,FORIN"]="4" ["41,INCLUDE"]="5" ["41,BUILTIN"]="6"
    ["41,QUOTE"]="7" ["41,VAR"]="8" ["41,STR"]="9" ["41,ARGS"]="71"
    ["42,ld"]="r ARGS ARGS STMT" ["42,cl"]="r ARGS ARGS STMT"
    ["42,rd"]="r ARGS ARGS STMT" ["42,or"]="r ARGS ARGS STMT"
    ["42,and"]="r ARGS ARGS STMT" ["42,rp"]="r ARGS ARGS STMT"
    ["42,ne"]="r ARGS ARGS STMT" ["42,eq"]="r ARGS ARGS STMT"
    ["42,gt"]="r ARGS ARGS STMT" ["42,lt"]="r ARGS ARGS STMT"
    ["42,ge"]="r ARGS ARGS STMT" ["42,le"]="r ARGS ARGS STMT"
    ["42,strne"]="r ARGS ARGS STMT" ["42,streq"]="r ARGS ARGS STMT"
    ["42,strgt"]="r ARGS ARGS STMT" ["42,strlt"]="r ARGS ARGS STMT"
    ["42,strcm"]="r ARGS ARGS STMT" ["42,str"]="r ARGS ARGS STMT"
    ["43,ld"]="r BOP ne" ["43,str"]="r BOP ne" ["44,ld"]="r BOP eq"
    ["44,str"]="r BOP eq" ["45,ld"]="r BOP gt" ["45,str"]="r BOP gt"
    ["46,ld"]="r BOP lt" ["46,str"]="r BOP lt" ["47,ld"]="r BOP ge"
    ["47,str"]="r BOP ge" ["48,ld"]="r BOP le" ["48,str"]="r BOP le"
    ["49,ld"]="r BOP strne" ["49,str"]="r BOP strne" ["50,ld"]="r BOP streq"
    ["50,str"]="r BOP streq" ["51,ld"]="r BOP strgt" ["51,str"]="r BOP strgt"
    ["52,ld"]="r BOP strlt" ["52,str"]="r BOP strlt" ["53,ld"]="r BOP strcm"
    ["53,str"]="r BOP strcm" ["54,ld"]="r DOC" ["54,rd"]="r DOC" ["54,elif"]="r DOC"
    ["54,else"]="r DOC" ["54,str"]="r DOC" ["54,DOC"]="72" ["55,ld"]="s 10"
    ["55,cl"]="s 73" ["55,rd"]="s 74" ["55,str"]="s 11" ["55,STMT"]="2"
    ["55,IF"]="3" ["55,FORIN"]="4" ["55,INCLUDE"]="5" ["55,BUILTIN"]="6"
    ["55,QUOTE"]="7" ["55,VAR"]="8" ["55,STR"]="9" ["56,ld"]="s 10"
    ["56,str"]="s 11" ["56,STMT"]="26" ["56,IF"]="3" ["56,FORIN"]="4"
    ["56,INCLUDE"]="5" ["56,BUILTIN"]="6" ["56,QUOTE"]="7" ["56,VAR"]="8"
    ["56,STR"]="9" ["56,ARGS"]="75" ["57,rd"]="s 76" ["58,ld"]="s 10"
    ["58,rd"]="s 77" ["58,str"]="s 11" ["58,STMT"]="42" ["58,IF"]="3"
    ["58,FORIN"]="4" ["58,INCLUDE"]="5" ["58,BUILTIN"]="6" ["58,QUOTE"]="7"
    ["58,VAR"]="8" ["58,STR"]="9" ["59,rd"]="s 78" ["60,rd"]="s 79" ["61,id"]="s 20"
    ["61,ID"]="80" ["62,rd"]="s 81" ["63,rd"]="s 82" ["64,ld"]="s 10"
    ["64,rd"]="s 83" ["64,str"]="s 11" ["64,STMT"]="42" ["64,IF"]="3"
    ["64,FORIN"]="4" ["64,INCLUDE"]="5" ["64,BUILTIN"]="6" ["64,QUOTE"]="7"
    ["64,VAR"]="8" ["64,STR"]="9" ["65,cl"]="r BOOLO BOOLO or BOOLA"
    ["65,rd"]="r BOOLO BOOLO or BOOLA" ["65,or"]="r BOOLO BOOLO or BOOLA"
    ["65,and"]="s 39" ["65,rp"]="r BOOLO BOOLO or BOOLA" ["66,ld"]="s 10"
    ["66,lp"]="s 23" ["66,str"]="s 11" ["66,STMT"]="26" ["66,IF"]="3"
    ["66,FORIN"]="4" ["66,INCLUDE"]="5" ["66,BUILTIN"]="6" ["66,QUOTE"]="7"
    ["66,VAR"]="8" ["66,STR"]="9" ["66,BOOLA"]="84" ["66,BOOL"]="24"
    ["66,ARGS"]="25" ["67,cl"]="r BOOLA BOOLA and BOOL"
    ["67,rd"]="r BOOLA BOOLA and BOOL" ["67,or"]="r BOOLA BOOLA and BOOL"
    ["67,and"]="r BOOLA BOOLA and BOOL" ["67,rp"]="r BOOLA BOOLA and BOOL"
    ["68,ld"]="s 10" ["68,lp"]="s 86" ["68,str"]="s 11" ["68,STMT"]="26"
    ["68,IF"]="3" ["68,FORIN"]="4" ["68,INCLUDE"]="5" ["68,BUILTIN"]="6"
    ["68,QUOTE"]="7" ["68,VAR"]="8" ["68,STR"]="9" ["68,BOOL"]="85" ["68,ARGS"]="25"
    ["69,ld"]="s 10" ["69,lp"]="s 23" ["69,ex"]="s 22" ["69,str"]="s 11"
    ["69,STMT"]="26" ["69,IF"]="3" ["69,FORIN"]="4" ["69,INCLUDE"]="5"
    ["69,BUILTIN"]="6" ["69,QUOTE"]="7" ["69,VAR"]="8" ["69,STR"]="9"
    ["69,BOOLS"]="87" ["69,BOOLO"]="18" ["69,UOP"]="19" ["69,BOOLA"]="21"
    ["69,BOOL"]="24" ["69,ARGS"]="25" ["70,cl"]="r BOOLA lp BOOLS rp"
    ["70,rd"]="r BOOLA lp BOOLS rp" ["70,or"]="r BOOLA lp BOOLS rp"
    ["70,and"]="r BOOLA lp BOOLS rp" ["70,rp"]="r BOOLA lp BOOLS rp"
    ["71,ld"]="s 10" ["71,cl"]="r BOOL ARGS BOP ARGS"
    ["71,rd"]="r BOOL ARGS BOP ARGS" ["71,or"]="r BOOL ARGS BOP ARGS"
    ["71,and"]="r BOOL ARGS BOP ARGS" ["71,rp"]="r BOOL ARGS BOP ARGS"
    ["71,str"]="s 11" ["71,STMT"]="42" ["71,IF"]="3" ["71,FORIN"]="4"
    ["71,INCLUDE"]="5" ["71,BUILTIN"]="6" ["71,QUOTE"]="7" ["71,VAR"]="8"
    ["71,STR"]="9" ["72,ld"]="s 10" ["72,rd"]="r ELIF" ["72,elif"]="r ELIF"
    ["72,else"]="r ELIF" ["72,str"]="s 11" ["72,STMT"]="2" ["72,IF"]="3"
    ["72,FORIN"]="4" ["72,INCLUDE"]="5" ["72,BUILTIN"]="6" ["72,QUOTE"]="7"
    ["72,VAR"]="8" ["72,STR"]="9" ["72,ELIF"]="88" ["73,ld"]="r DOC"
    ["73,rd"]="r DOC" ["73,str"]="r DOC" ["73,DOC"]="89"
    ["74,ld"]="r IF ld BOOLS cl DOC rd" ["74,cl"]="r IF ld BOOLS cl DOC rd"
    ["74,rd"]="r IF ld BOOLS cl DOC rd" ["74,elif"]="r IF ld BOOLS cl DOC rd"
    ["74,else"]="r IF ld BOOLS cl DOC rd" ["74,or"]="r IF ld BOOLS cl DOC rd"
    ["74,and"]="r IF ld BOOLS cl DOC rd" ["74,rp"]="r IF ld BOOLS cl DOC rd"
    ["74,ne"]="r IF ld BOOLS cl DOC rd" ["74,eq"]="r IF ld BOOLS cl DOC rd"
    ["74,gt"]="r IF ld BOOLS cl DOC rd" ["74,lt"]="r IF ld BOOLS cl DOC rd"
    ["74,ge"]="r IF ld BOOLS cl DOC rd" ["74,le"]="r IF ld BOOLS cl DOC rd"
    ["74,strne"]="r IF ld BOOLS cl DOC rd" ["74,streq"]="r IF ld BOOLS cl DOC rd"
    ["74,strgt"]="r IF ld BOOLS cl DOC rd" ["74,strlt"]="r IF ld BOOLS cl DOC rd"
    ["74,strcm"]="r IF ld BOOLS cl DOC rd" ["74,str"]="r IF ld BOOLS cl DOC rd"
    ["74,$"]="r IF ld BOOLS cl DOC rd" ["75,ld"]="s 10" ["75,cl"]="s 90"
    ["75,str"]="s 11" ["75,STMT"]="42" ["75,IF"]="3" ["75,FORIN"]="4"
    ["75,INCLUDE"]="5" ["75,BUILTIN"]="6" ["75,QUOTE"]="7" ["75,VAR"]="8"
    ["75,STR"]="9" ["76,ld"]="r INCLUDE ld include cl STR rd"
    ["76,cl"]="r INCLUDE ld include cl STR rd"
    ["76,rd"]="r INCLUDE ld include cl STR rd"
    ["76,elif"]="r INCLUDE ld include cl STR rd"
    ["76,else"]="r INCLUDE ld include cl STR rd"
    ["76,or"]="r INCLUDE ld include cl STR rd"
    ["76,and"]="r INCLUDE ld include cl STR rd"
    ["76,rp"]="r INCLUDE ld include cl STR rd"
    ["76,ne"]="r INCLUDE ld include cl STR rd"
    ["76,eq"]="r INCLUDE ld include cl STR rd"
    ["76,gt"]="r INCLUDE ld include cl STR rd"
    ["76,lt"]="r INCLUDE ld include cl STR rd"
    ["76,ge"]="r INCLUDE ld include cl STR rd"
    ["76,le"]="r INCLUDE ld include cl STR rd"
    ["76,strne"]="r INCLUDE ld include cl STR rd"
    ["76,streq"]="r INCLUDE ld include cl STR rd"
    ["76,strgt"]="r INCLUDE ld include cl STR rd"
    ["76,strlt"]="r INCLUDE ld include cl STR rd"
    ["76,strcm"]="r INCLUDE ld include cl STR rd"
    ["76,str"]="r INCLUDE ld include cl STR rd"
    ["76,$"]="r INCLUDE ld include cl STR rd" ["77,ld"]="r BUILTIN ld ID cl ARGS rd"
    ["77,cl"]="r BUILTIN ld ID cl ARGS rd" ["77,rd"]="r BUILTIN ld ID cl ARGS rd"
    ["77,elif"]="r BUILTIN ld ID cl ARGS rd"
    ["77,else"]="r BUILTIN ld ID cl ARGS rd" ["77,or"]="r BUILTIN ld ID cl ARGS rd"
    ["77,and"]="r BUILTIN ld ID cl ARGS rd" ["77,rp"]="r BUILTIN ld ID cl ARGS rd"
    ["77,ne"]="r BUILTIN ld ID cl ARGS rd" ["77,eq"]="r BUILTIN ld ID cl ARGS rd"
    ["77,gt"]="r BUILTIN ld ID cl ARGS rd" ["77,lt"]="r BUILTIN ld ID cl ARGS rd"
    ["77,ge"]="r BUILTIN ld ID cl ARGS rd" ["77,le"]="r BUILTIN ld ID cl ARGS rd"
    ["77,strne"]="r BUILTIN ld ID cl ARGS rd"
    ["77,streq"]="r BUILTIN ld ID cl ARGS rd"
    ["77,strgt"]="r BUILTIN ld ID cl ARGS rd"
    ["77,strlt"]="r BUILTIN ld ID cl ARGS rd"
    ["77,strcm"]="r BUILTIN ld ID cl ARGS rd"
    ["77,str"]="r BUILTIN ld ID cl ARGS rd" ["77,$"]="r BUILTIN ld ID cl ARGS rd"
    ["78,ld"]="r VAR ld ID or VAR rd" ["78,cl"]="r VAR ld ID or VAR rd"
    ["78,rd"]="r VAR ld ID or VAR rd" ["78,elif"]="r VAR ld ID or VAR rd"
    ["78,else"]="r VAR ld ID or VAR rd" ["78,or"]="r VAR ld ID or VAR rd"
    ["78,and"]="r VAR ld ID or VAR rd" ["78,rp"]="r VAR ld ID or VAR rd"
    ["78,ne"]="r VAR ld ID or VAR rd" ["78,eq"]="r VAR ld ID or VAR rd"
    ["78,gt"]="r VAR ld ID or VAR rd" ["78,lt"]="r VAR ld ID or VAR rd"
    ["78,ge"]="r VAR ld ID or VAR rd" ["78,le"]="r VAR ld ID or VAR rd"
    ["78,strne"]="r VAR ld ID or VAR rd" ["78,streq"]="r VAR ld ID or VAR rd"
    ["78,strgt"]="r VAR ld ID or VAR rd" ["78,strlt"]="r VAR ld ID or VAR rd"
    ["78,strcm"]="r VAR ld ID or VAR rd" ["78,str"]="r VAR ld ID or VAR rd"
    ["78,$"]="r VAR ld ID or VAR rd" ["79,ld"]="r VAR ld ID or STR rd"
    ["79,cl"]="r VAR ld ID or STR rd" ["79,rd"]="r VAR ld ID or STR rd"
    ["79,elif"]="r VAR ld ID or STR rd" ["79,else"]="r VAR ld ID or STR rd"
    ["79,or"]="r VAR ld ID or STR rd" ["79,and"]="r VAR ld ID or STR rd"
    ["79,rp"]="r VAR ld ID or STR rd" ["79,ne"]="r VAR ld ID or STR rd"
    ["79,eq"]="r VAR ld ID or STR rd" ["79,gt"]="r VAR ld ID or STR rd"
    ["79,lt"]="r VAR ld ID or STR rd" ["79,ge"]="r VAR ld ID or STR rd"
    ["79,le"]="r VAR ld ID or STR rd" ["79,strne"]="r VAR ld ID or STR rd"
    ["79,streq"]="r VAR ld ID or STR rd" ["79,strgt"]="r VAR ld ID or STR rd"
    ["79,strlt"]="r VAR ld ID or STR rd" ["79,strcm"]="r VAR ld ID or STR rd"
    ["79,str"]="r VAR ld ID or STR rd" ["79,$"]="r VAR ld ID or STR rd"
    ["80,rd"]="s 33" ["80,or"]="s 34" ["80,and"]="s 35"
    ["81,ld"]="r VAR ld ID and VAR rd" ["81,cl"]="r VAR ld ID and VAR rd"
    ["81,rd"]="r VAR ld ID and VAR rd" ["81,elif"]="r VAR ld ID and VAR rd"
    ["81,else"]="r VAR ld ID and VAR rd" ["81,or"]="r VAR ld ID and VAR rd"
    ["81,and"]="r VAR ld ID and VAR rd" ["81,rp"]="r VAR ld ID and VAR rd"
    ["81,ne"]="r VAR ld ID and VAR rd" ["81,eq"]="r VAR ld ID and VAR rd"
    ["81,gt"]="r VAR ld ID and VAR rd" ["81,lt"]="r VAR ld ID and VAR rd"
    ["81,ge"]="r VAR ld ID and VAR rd" ["81,le"]="r VAR ld ID and VAR rd"
    ["81,strne"]="r VAR ld ID and VAR rd" ["81,streq"]="r VAR ld ID and VAR rd"
    ["81,strgt"]="r VAR ld ID and VAR rd" ["81,strlt"]="r VAR ld ID and VAR rd"
    ["81,strcm"]="r VAR ld ID and VAR rd" ["81,str"]="r VAR ld ID and VAR rd"
    ["81,$"]="r VAR ld ID and VAR rd" ["82,ld"]="r VAR ld ID and STR rd"
    ["82,cl"]="r VAR ld ID and STR rd" ["82,rd"]="r VAR ld ID and STR rd"
    ["82,elif"]="r VAR ld ID and STR rd" ["82,else"]="r VAR ld ID and STR rd"
    ["82,or"]="r VAR ld ID and STR rd" ["82,and"]="r VAR ld ID and STR rd"
    ["82,rp"]="r VAR ld ID and STR rd" ["82,ne"]="r VAR ld ID and STR rd"
    ["82,eq"]="r VAR ld ID and STR rd" ["82,gt"]="r VAR ld ID and STR rd"
    ["82,lt"]="r VAR ld ID and STR rd" ["82,ge"]="r VAR ld ID and STR rd"
    ["82,le"]="r VAR ld ID and STR rd" ["82,strne"]="r VAR ld ID and STR rd"
    ["82,streq"]="r VAR ld ID and STR rd" ["82,strgt"]="r VAR ld ID and STR rd"
    ["82,strlt"]="r VAR ld ID and STR rd" ["82,strcm"]="r VAR ld ID and STR rd"
    ["82,str"]="r VAR ld ID and STR rd" ["82,$"]="r VAR ld ID and STR rd"
    ["83,ld"]="r QUOTE ld quote cl ARGS rd" ["83,cl"]="r QUOTE ld quote cl ARGS rd"
    ["83,rd"]="r QUOTE ld quote cl ARGS rd"
    ["83,elif"]="r QUOTE ld quote cl ARGS rd"
    ["83,else"]="r QUOTE ld quote cl ARGS rd"
    ["83,or"]="r QUOTE ld quote cl ARGS rd" ["83,and"]="r QUOTE ld quote cl ARGS rd"
    ["83,rp"]="r QUOTE ld quote cl ARGS rd" ["83,ne"]="r QUOTE ld quote cl ARGS rd"
    ["83,eq"]="r QUOTE ld quote cl ARGS rd" ["83,gt"]="r QUOTE ld quote cl ARGS rd"
    ["83,lt"]="r QUOTE ld quote cl ARGS rd" ["83,ge"]="r QUOTE ld quote cl ARGS rd"
    ["83,le"]="r QUOTE ld quote cl ARGS rd"
    ["83,strne"]="r QUOTE ld quote cl ARGS rd"
    ["83,streq"]="r QUOTE ld quote cl ARGS rd"
    ["83,strgt"]="r QUOTE ld quote cl ARGS rd"
    ["83,strlt"]="r QUOTE ld quote cl ARGS rd"
    ["83,strcm"]="r QUOTE ld quote cl ARGS rd"
    ["83,str"]="r QUOTE ld quote cl ARGS rd" ["83,$"]="r QUOTE ld quote cl ARGS rd"
    ["84,cl"]="r BOOLO BOOLO or UOP BOOLA" ["84,rd"]="r BOOLO BOOLO or UOP BOOLA"
    ["84,or"]="r BOOLO BOOLO or UOP BOOLA" ["84,and"]="s 39"
    ["84,rp"]="r BOOLO BOOLO or UOP BOOLA" ["85,cl"]="r BOOLA BOOLA and UOP BOOL"
    ["85,rd"]="r BOOLA BOOLA and UOP BOOL" ["85,or"]="r BOOLA BOOLA and UOP BOOL"
    ["85,and"]="r BOOLA BOOLA and UOP BOOL" ["85,rp"]="r BOOLA BOOLA and UOP BOOL"
    ["86,ld"]="s 10" ["86,lp"]="s 23" ["86,ex"]="s 22" ["86,str"]="s 11"
    ["86,STMT"]="26" ["86,IF"]="3" ["86,FORIN"]="4" ["86,INCLUDE"]="5"
    ["86,BUILTIN"]="6" ["86,QUOTE"]="7" ["86,VAR"]="8" ["86,STR"]="9"
    ["86,BOOLS"]="91" ["86,BOOLO"]="18" ["86,UOP"]="19" ["86,BOOLA"]="21"
    ["86,BOOL"]="24" ["86,ARGS"]="25" ["87,rp"]="s 92" ["88,rd"]="r ELSE"
    ["88,elif"]="s 94" ["88,else"]="s 95" ["88,ELSE"]="93" ["89,ld"]="s 10"
    ["89,rd"]="s 96" ["89,str"]="s 11" ["89,STMT"]="2" ["89,IF"]="3"
    ["89,FORIN"]="4" ["89,INCLUDE"]="5" ["89,BUILTIN"]="6" ["89,QUOTE"]="7"
    ["89,VAR"]="8" ["89,STR"]="9" ["90,ld"]="r DOC" ["90,rd"]="r DOC"
    ["90,str"]="r DOC" ["90,DOC"]="97" ["91,rp"]="s 98"
    ["92,cl"]="r BOOLA BOOLA and lp BOOLS rp"
    ["92,rd"]="r BOOLA BOOLA and lp BOOLS rp"
    ["92,or"]="r BOOLA BOOLA and lp BOOLS rp"
    ["92,and"]="r BOOLA BOOLA and lp BOOLS rp"
    ["92,rp"]="r BOOLA BOOLA and lp BOOLS rp" ["93,rd"]="s 99" ["94,ld"]="s 10"
    ["94,lp"]="s 23" ["94,ex"]="s 22" ["94,str"]="s 11" ["94,STMT"]="26"
    ["94,IF"]="3" ["94,FORIN"]="4" ["94,INCLUDE"]="5" ["94,BUILTIN"]="6"
    ["94,QUOTE"]="7" ["94,VAR"]="8" ["94,STR"]="9" ["94,BOOLS"]="100"
    ["94,BOOLO"]="18" ["94,UOP"]="19" ["94,BOOLA"]="21" ["94,BOOL"]="24"
    ["94,ARGS"]="25" ["95,cl"]="s 101" ["96,ld"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,cl"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,rd"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,elif"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,else"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,or"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,and"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,rp"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,ne"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,eq"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,gt"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,lt"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,ge"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,le"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,strne"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,streq"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,strgt"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,strlt"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,strcm"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,str"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["96,$"]="r IF ld BOOLS cl DOC cl DOC rd" ["97,ld"]="s 10" ["97,rd"]="s 102"
    ["97,str"]="s 11" ["97,STMT"]="2" ["97,IF"]="3" ["97,FORIN"]="4"
    ["97,INCLUDE"]="5" ["97,BUILTIN"]="6" ["97,QUOTE"]="7" ["97,VAR"]="8"
    ["97,STR"]="9" ["98,cl"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["98,rd"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["98,or"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["98,and"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["98,rp"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["99,ld"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,cl"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,rd"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,elif"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,else"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,or"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,and"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,rp"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,ne"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,eq"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,gt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,lt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,ge"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,le"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,strne"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,streq"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,strgt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,strlt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,strcm"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,str"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["99,$"]="r IF ld if BOOLS cl DOC ELIF ELSE rd" ["100,cl"]="s 103"
    ["101,ld"]="r DOC" ["101,rd"]="r DOC" ["101,str"]="r DOC" ["101,DOC"]="104"
    ["102,ld"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,cl"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,rd"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,elif"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,else"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,or"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,and"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,rp"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,ne"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,eq"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,gt"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,lt"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,ge"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,le"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,strne"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,streq"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,strgt"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,strlt"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,strcm"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,str"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["102,$"]="r FORIN ld for ID in ARGS cl DOC rd" ["103,ld"]="r DOC"
    ["103,rd"]="r DOC" ["103,elif"]="r DOC" ["103,else"]="r DOC" ["103,str"]="r DOC"
    ["103,DOC"]="105" ["104,ld"]="s 10" ["104,rd"]="r ELSE else cl DOC"
    ["104,str"]="s 11" ["104,STMT"]="2" ["104,IF"]="3" ["104,FORIN"]="4"
    ["104,INCLUDE"]="5" ["104,BUILTIN"]="6" ["104,QUOTE"]="7" ["104,VAR"]="8"
    ["104,STR"]="9" ["105,ld"]="s 10" ["105,rd"]="r ELIF ELIF elif BOOLS cl DOC"
    ["105,elif"]="r ELIF ELIF elif BOOLS cl DOC"
    ["105,else"]="r ELIF ELIF elif BOOLS cl DOC" ["105,str"]="s 11" ["105,STMT"]="2"
    ["105,IF"]="3" ["105,FORIN"]="4" ["105,INCLUDE"]="5" ["105,BUILTIN"]="6"
    ["105,QUOTE"]="7" ["105,VAR"]="8" ["105,STR"]="9"
) # }}}
# <<< BPT_PARSE_TABLE_E <<<

return 0 2>/dev/null || bpt.main "$@"
