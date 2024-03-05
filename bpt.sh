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
#   and|or|if|elif|else|for|in|include: <as is>
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

    local -a KW=('==' '!=' '>' '<' ':' '\!' '"' "'" '\(' '\)') # Keywords
    local -a SKW=( # Keywords ending with an alphanumeric character
        '-eq' '-ne' '-gt' '-lt' '-ge' '-le'
        'and' 'or' 'if' 'elif' 'else' 'for' 'in' 'include'
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
                    '!') echo -n ex ;; ':') echo -n cl ;;
                    '(') echo -n lp ;; ')') echo -n rp ;;
                    and | or | if | elif | else) ;&
                    for | in | include) echo -n "$content" ;;
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
    BUILTIN) contents[$s]="${contents[$((s + 3))]}" ;;
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
        VAR | BUILTIN) contents[$s]+=" $stmt " ;;
        INCLUDE | FORIN | IF) contents[$s]+=" \"\$($stmt)\" " ;;
        esac
        ;;
    BUILTIN) # Filter allowed builtints
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
    INCLUDE) contents[$s]="$(bpt.__recursive_process "${contents[$((s + 3))]}")" ;;
    FORIN) contents[$s]="for ${contents[$((s + 2))]%:*:*} in ${contents[$((s + 4))]}; do ${contents[$((s + 6))]} done" ;;
    BOOL) case "${#rule[@]}" in
        2) contents[$s]="\"\$(e ${contents[$s]})\"" ;;
        4) contents[$s]+=" ${contents[$((s + 1))]} \"\$(e ${contents[$((s + 2))]})\"" ;;
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
    echo "      compare strings:   >, <, ==, !="
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
    ["0,ld"]="r DOC" ["0,str"]="r DOC" ["0,$"]="r DOC" ["0,DOC"]="1" ["1,ld"]="s 9"
    ["1,str"]="s 10" ["1,$"]="a" ["1,STMT"]="2" ["1,IF"]="3" ["1,FORIN"]="4"
    ["1,INCLUDE"]="5" ["1,BUILTIN"]="6" ["1,VAR"]="7" ["1,STR"]="8"
    ["2,ld"]="r DOC DOC STMT" ["2,cl"]="r DOC DOC STMT" ["2,rd"]="r DOC DOC STMT"
    ["2,elif"]="r DOC DOC STMT" ["2,else"]="r DOC DOC STMT"
    ["2,str"]="r DOC DOC STMT" ["2,$"]="r DOC DOC STMT" ["3,ld"]="r STMT IF"
    ["3,cl"]="r STMT IF" ["3,rd"]="r STMT IF" ["3,elif"]="r STMT IF"
    ["3,else"]="r STMT IF" ["3,or"]="r STMT IF" ["3,and"]="r STMT IF"
    ["3,rp"]="r STMT IF" ["3,ne"]="r STMT IF" ["3,eq"]="r STMT IF"
    ["3,gt"]="r STMT IF" ["3,lt"]="r STMT IF" ["3,ge"]="r STMT IF"
    ["3,le"]="r STMT IF" ["3,strgt"]="r STMT IF" ["3,strlt"]="r STMT IF"
    ["3,streq"]="r STMT IF" ["3,strne"]="r STMT IF" ["3,str"]="r STMT IF"
    ["3,$"]="r STMT IF" ["4,ld"]="r STMT FORIN" ["4,cl"]="r STMT FORIN"
    ["4,rd"]="r STMT FORIN" ["4,elif"]="r STMT FORIN" ["4,else"]="r STMT FORIN"
    ["4,or"]="r STMT FORIN" ["4,and"]="r STMT FORIN" ["4,rp"]="r STMT FORIN"
    ["4,ne"]="r STMT FORIN" ["4,eq"]="r STMT FORIN" ["4,gt"]="r STMT FORIN"
    ["4,lt"]="r STMT FORIN" ["4,ge"]="r STMT FORIN" ["4,le"]="r STMT FORIN"
    ["4,strgt"]="r STMT FORIN" ["4,strlt"]="r STMT FORIN" ["4,streq"]="r STMT FORIN"
    ["4,strne"]="r STMT FORIN" ["4,str"]="r STMT FORIN" ["4,$"]="r STMT FORIN"
    ["5,ld"]="r STMT INCLUDE" ["5,cl"]="r STMT INCLUDE" ["5,rd"]="r STMT INCLUDE"
    ["5,elif"]="r STMT INCLUDE" ["5,else"]="r STMT INCLUDE"
    ["5,or"]="r STMT INCLUDE" ["5,and"]="r STMT INCLUDE" ["5,rp"]="r STMT INCLUDE"
    ["5,ne"]="r STMT INCLUDE" ["5,eq"]="r STMT INCLUDE" ["5,gt"]="r STMT INCLUDE"
    ["5,lt"]="r STMT INCLUDE" ["5,ge"]="r STMT INCLUDE" ["5,le"]="r STMT INCLUDE"
    ["5,strgt"]="r STMT INCLUDE" ["5,strlt"]="r STMT INCLUDE"
    ["5,streq"]="r STMT INCLUDE" ["5,strne"]="r STMT INCLUDE"
    ["5,str"]="r STMT INCLUDE" ["5,$"]="r STMT INCLUDE" ["6,ld"]="r STMT BUILTIN"
    ["6,cl"]="r STMT BUILTIN" ["6,rd"]="r STMT BUILTIN" ["6,elif"]="r STMT BUILTIN"
    ["6,else"]="r STMT BUILTIN" ["6,or"]="r STMT BUILTIN" ["6,and"]="r STMT BUILTIN"
    ["6,rp"]="r STMT BUILTIN" ["6,ne"]="r STMT BUILTIN" ["6,eq"]="r STMT BUILTIN"
    ["6,gt"]="r STMT BUILTIN" ["6,lt"]="r STMT BUILTIN" ["6,ge"]="r STMT BUILTIN"
    ["6,le"]="r STMT BUILTIN" ["6,strgt"]="r STMT BUILTIN"
    ["6,strlt"]="r STMT BUILTIN" ["6,streq"]="r STMT BUILTIN"
    ["6,strne"]="r STMT BUILTIN" ["6,str"]="r STMT BUILTIN" ["6,$"]="r STMT BUILTIN"
    ["7,ld"]="r STMT VAR" ["7,cl"]="r STMT VAR" ["7,rd"]="r STMT VAR"
    ["7,elif"]="r STMT VAR" ["7,else"]="r STMT VAR" ["7,or"]="r STMT VAR"
    ["7,and"]="r STMT VAR" ["7,rp"]="r STMT VAR" ["7,ne"]="r STMT VAR"
    ["7,eq"]="r STMT VAR" ["7,gt"]="r STMT VAR" ["7,lt"]="r STMT VAR"
    ["7,ge"]="r STMT VAR" ["7,le"]="r STMT VAR" ["7,strgt"]="r STMT VAR"
    ["7,strlt"]="r STMT VAR" ["7,streq"]="r STMT VAR" ["7,strne"]="r STMT VAR"
    ["7,str"]="r STMT VAR" ["7,$"]="r STMT VAR" ["8,ld"]="r STMT STR"
    ["8,cl"]="r STMT STR" ["8,rd"]="r STMT STR" ["8,elif"]="r STMT STR"
    ["8,else"]="r STMT STR" ["8,or"]="r STMT STR" ["8,and"]="r STMT STR"
    ["8,rp"]="r STMT STR" ["8,ne"]="r STMT STR" ["8,eq"]="r STMT STR"
    ["8,gt"]="r STMT STR" ["8,lt"]="r STMT STR" ["8,ge"]="r STMT STR"
    ["8,le"]="r STMT STR" ["8,strgt"]="r STMT STR" ["8,strlt"]="r STMT STR"
    ["8,streq"]="r STMT STR" ["8,strne"]="r STMT STR" ["8,str"]="r STMT STR"
    ["8,$"]="r STMT STR" ["9,ld"]="s 9" ["9,if"]="s 11" ["9,lp"]="s 21"
    ["9,for"]="s 13" ["9,include"]="s 14" ["9,ex"]="s 20" ["9,id"]="s 18"
    ["9,str"]="s 10" ["9,STMT"]="24" ["9,IF"]="3" ["9,FORIN"]="4" ["9,INCLUDE"]="5"
    ["9,BUILTIN"]="6" ["9,VAR"]="7" ["9,STR"]="8" ["9,BOOLS"]="12" ["9,BOOLO"]="16"
    ["9,UOP"]="17" ["9,BOOLA"]="19" ["9,BOOL"]="22" ["9,ARGS"]="23" ["9,ID"]="15"
    ["10,ld"]="r STR str" ["10,cl"]="r STR str" ["10,rd"]="r STR str"
    ["10,elif"]="r STR str" ["10,else"]="r STR str" ["10,or"]="r STR str"
    ["10,and"]="r STR str" ["10,rp"]="r STR str" ["10,ne"]="r STR str"
    ["10,eq"]="r STR str" ["10,gt"]="r STR str" ["10,lt"]="r STR str"
    ["10,ge"]="r STR str" ["10,le"]="r STR str" ["10,strgt"]="r STR str"
    ["10,strlt"]="r STR str" ["10,streq"]="r STR str" ["10,strne"]="r STR str"
    ["10,str"]="r STR str" ["10,$"]="r STR str" ["11,ld"]="s 9" ["11,lp"]="s 21"
    ["11,ex"]="s 20" ["11,str"]="s 10" ["11,STMT"]="24" ["11,IF"]="3"
    ["11,FORIN"]="4" ["11,INCLUDE"]="5" ["11,BUILTIN"]="6" ["11,VAR"]="7"
    ["11,STR"]="8" ["11,BOOLS"]="25" ["11,BOOLO"]="16" ["11,UOP"]="17"
    ["11,BOOLA"]="19" ["11,BOOL"]="22" ["11,ARGS"]="23" ["12,cl"]="s 26"
    ["12,rd"]="s 27" ["13,id"]="s 18" ["13,ID"]="28" ["14,cl"]="s 29"
    ["15,cl"]="s 30" ["15,rd"]="s 31" ["15,or"]="s 32" ["15,and"]="s 33"
    ["16,cl"]="r BOOLS BOOLO" ["16,rd"]="r BOOLS BOOLO" ["16,or"]="s 34"
    ["16,rp"]="r BOOLS BOOLO" ["17,ld"]="s 9" ["17,lp"]="s 21" ["17,ex"]="s 20"
    ["17,str"]="s 10" ["17,STMT"]="24" ["17,IF"]="3" ["17,FORIN"]="4"
    ["17,INCLUDE"]="5" ["17,BUILTIN"]="6" ["17,VAR"]="7" ["17,STR"]="8"
    ["17,BOOLS"]="35" ["17,BOOLO"]="16" ["17,UOP"]="17" ["17,BOOLA"]="19"
    ["17,BOOL"]="22" ["17,ARGS"]="23" ["18,cl"]="r ID id" ["18,rd"]="r ID id"
    ["18,or"]="r ID id" ["18,and"]="r ID id" ["18,in"]="r ID id"
    ["19,cl"]="r BOOLO BOOLA" ["19,rd"]="r BOOLO BOOLA" ["19,or"]="r BOOLO BOOLA"
    ["19,and"]="s 36" ["19,rp"]="r BOOLO BOOLA" ["20,ld"]="r UOP ex"
    ["20,lp"]="r UOP ex" ["20,ex"]="r UOP ex" ["20,str"]="r UOP ex" ["21,ld"]="s 9"
    ["21,lp"]="s 21" ["21,ex"]="s 20" ["21,str"]="s 10" ["21,STMT"]="24"
    ["21,IF"]="3" ["21,FORIN"]="4" ["21,INCLUDE"]="5" ["21,BUILTIN"]="6"
    ["21,VAR"]="7" ["21,STR"]="8" ["21,BOOLS"]="37" ["21,BOOLO"]="16"
    ["21,UOP"]="17" ["21,BOOLA"]="19" ["21,BOOL"]="22" ["21,ARGS"]="23"
    ["22,cl"]="r BOOLA BOOL" ["22,rd"]="r BOOLA BOOL" ["22,or"]="r BOOLA BOOL"
    ["22,and"]="r BOOLA BOOL" ["22,rp"]="r BOOLA BOOL" ["23,ld"]="s 9"
    ["23,cl"]="r BOOL ARGS" ["23,rd"]="r BOOL ARGS" ["23,or"]="r BOOL ARGS"
    ["23,and"]="r BOOL ARGS" ["23,rp"]="r BOOL ARGS" ["23,ne"]="s 40"
    ["23,eq"]="s 41" ["23,gt"]="s 42" ["23,lt"]="s 43" ["23,ge"]="s 44"
    ["23,le"]="s 45" ["23,strgt"]="s 46" ["23,strlt"]="s 47" ["23,streq"]="s 48"
    ["23,strne"]="s 49" ["23,str"]="s 10" ["23,STMT"]="39" ["23,IF"]="3"
    ["23,FORIN"]="4" ["23,INCLUDE"]="5" ["23,BUILTIN"]="6" ["23,VAR"]="7"
    ["23,STR"]="8" ["23,BOP"]="38" ["24,ld"]="r ARGS STMT" ["24,cl"]="r ARGS STMT"
    ["24,rd"]="r ARGS STMT" ["24,or"]="r ARGS STMT" ["24,and"]="r ARGS STMT"
    ["24,rp"]="r ARGS STMT" ["24,ne"]="r ARGS STMT" ["24,eq"]="r ARGS STMT"
    ["24,gt"]="r ARGS STMT" ["24,lt"]="r ARGS STMT" ["24,ge"]="r ARGS STMT"
    ["24,le"]="r ARGS STMT" ["24,strgt"]="r ARGS STMT" ["24,strlt"]="r ARGS STMT"
    ["24,streq"]="r ARGS STMT" ["24,strne"]="r ARGS STMT" ["24,str"]="r ARGS STMT"
    ["25,cl"]="s 50" ["26,ld"]="r DOC" ["26,cl"]="r DOC" ["26,rd"]="r DOC"
    ["26,str"]="r DOC" ["26,DOC"]="51" ["27,ld"]="r IF ld BOOLS rd"
    ["27,cl"]="r IF ld BOOLS rd" ["27,rd"]="r IF ld BOOLS rd"
    ["27,elif"]="r IF ld BOOLS rd" ["27,else"]="r IF ld BOOLS rd"
    ["27,or"]="r IF ld BOOLS rd" ["27,and"]="r IF ld BOOLS rd"
    ["27,rp"]="r IF ld BOOLS rd" ["27,ne"]="r IF ld BOOLS rd"
    ["27,eq"]="r IF ld BOOLS rd" ["27,gt"]="r IF ld BOOLS rd"
    ["27,lt"]="r IF ld BOOLS rd" ["27,ge"]="r IF ld BOOLS rd"
    ["27,le"]="r IF ld BOOLS rd" ["27,strgt"]="r IF ld BOOLS rd"
    ["27,strlt"]="r IF ld BOOLS rd" ["27,streq"]="r IF ld BOOLS rd"
    ["27,strne"]="r IF ld BOOLS rd" ["27,str"]="r IF ld BOOLS rd"
    ["27,$"]="r IF ld BOOLS rd" ["28,in"]="s 52" ["29,str"]="s 10" ["29,STR"]="53"
    ["30,ld"]="s 9" ["30,str"]="s 10" ["30,STMT"]="24" ["30,IF"]="3"
    ["30,FORIN"]="4" ["30,INCLUDE"]="5" ["30,BUILTIN"]="6" ["30,VAR"]="7"
    ["30,STR"]="8" ["30,ARGS"]="54" ["31,ld"]="r VAR ld ID rd"
    ["31,cl"]="r VAR ld ID rd" ["31,rd"]="r VAR ld ID rd"
    ["31,elif"]="r VAR ld ID rd" ["31,else"]="r VAR ld ID rd"
    ["31,or"]="r VAR ld ID rd" ["31,and"]="r VAR ld ID rd"
    ["31,rp"]="r VAR ld ID rd" ["31,ne"]="r VAR ld ID rd" ["31,eq"]="r VAR ld ID rd"
    ["31,gt"]="r VAR ld ID rd" ["31,lt"]="r VAR ld ID rd" ["31,ge"]="r VAR ld ID rd"
    ["31,le"]="r VAR ld ID rd" ["31,strgt"]="r VAR ld ID rd"
    ["31,strlt"]="r VAR ld ID rd" ["31,streq"]="r VAR ld ID rd"
    ["31,strne"]="r VAR ld ID rd" ["31,str"]="r VAR ld ID rd"
    ["31,$"]="r VAR ld ID rd" ["32,ld"]="s 57" ["32,str"]="s 10" ["32,VAR"]="55"
    ["32,STR"]="56" ["33,ld"]="s 57" ["33,str"]="s 10" ["33,VAR"]="58"
    ["33,STR"]="59" ["34,ld"]="s 9" ["34,lp"]="s 21" ["34,ex"]="s 20"
    ["34,str"]="s 10" ["34,STMT"]="24" ["34,IF"]="3" ["34,FORIN"]="4"
    ["34,INCLUDE"]="5" ["34,BUILTIN"]="6" ["34,VAR"]="7" ["34,STR"]="8"
    ["34,UOP"]="61" ["34,BOOLA"]="60" ["34,BOOL"]="22" ["34,ARGS"]="23"
    ["35,cl"]="r BOOLS UOP BOOLS" ["35,rd"]="r BOOLS UOP BOOLS"
    ["35,rp"]="r BOOLS UOP BOOLS" ["36,ld"]="s 9" ["36,lp"]="s 64" ["36,ex"]="s 20"
    ["36,str"]="s 10" ["36,STMT"]="24" ["36,IF"]="3" ["36,FORIN"]="4"
    ["36,INCLUDE"]="5" ["36,BUILTIN"]="6" ["36,VAR"]="7" ["36,STR"]="8"
    ["36,UOP"]="63" ["36,BOOL"]="62" ["36,ARGS"]="23" ["37,rp"]="s 65"
    ["38,ld"]="s 9" ["38,str"]="s 10" ["38,STMT"]="24" ["38,IF"]="3"
    ["38,FORIN"]="4" ["38,INCLUDE"]="5" ["38,BUILTIN"]="6" ["38,VAR"]="7"
    ["38,STR"]="8" ["38,ARGS"]="66" ["39,ld"]="r ARGS ARGS STMT"
    ["39,cl"]="r ARGS ARGS STMT" ["39,rd"]="r ARGS ARGS STMT"
    ["39,or"]="r ARGS ARGS STMT" ["39,and"]="r ARGS ARGS STMT"
    ["39,rp"]="r ARGS ARGS STMT" ["39,ne"]="r ARGS ARGS STMT"
    ["39,eq"]="r ARGS ARGS STMT" ["39,gt"]="r ARGS ARGS STMT"
    ["39,lt"]="r ARGS ARGS STMT" ["39,ge"]="r ARGS ARGS STMT"
    ["39,le"]="r ARGS ARGS STMT" ["39,strgt"]="r ARGS ARGS STMT"
    ["39,strlt"]="r ARGS ARGS STMT" ["39,streq"]="r ARGS ARGS STMT"
    ["39,strne"]="r ARGS ARGS STMT" ["39,str"]="r ARGS ARGS STMT"
    ["40,ld"]="r BOP ne" ["40,str"]="r BOP ne" ["41,ld"]="r BOP eq"
    ["41,str"]="r BOP eq" ["42,ld"]="r BOP gt" ["42,str"]="r BOP gt"
    ["43,ld"]="r BOP lt" ["43,str"]="r BOP lt" ["44,ld"]="r BOP ge"
    ["44,str"]="r BOP ge" ["45,ld"]="r BOP le" ["45,str"]="r BOP le"
    ["46,ld"]="r BOP strgt" ["46,str"]="r BOP strgt" ["47,ld"]="r BOP strlt"
    ["47,str"]="r BOP strlt" ["48,ld"]="r BOP streq" ["48,str"]="r BOP streq"
    ["49,ld"]="r BOP strne" ["49,str"]="r BOP strne" ["50,ld"]="r DOC"
    ["50,rd"]="r DOC" ["50,elif"]="r DOC" ["50,else"]="r DOC" ["50,str"]="r DOC"
    ["50,DOC"]="67" ["51,ld"]="s 9" ["51,cl"]="s 68" ["51,rd"]="s 69"
    ["51,str"]="s 10" ["51,STMT"]="2" ["51,IF"]="3" ["51,FORIN"]="4"
    ["51,INCLUDE"]="5" ["51,BUILTIN"]="6" ["51,VAR"]="7" ["51,STR"]="8"
    ["52,ld"]="s 9" ["52,str"]="s 10" ["52,STMT"]="24" ["52,IF"]="3"
    ["52,FORIN"]="4" ["52,INCLUDE"]="5" ["52,BUILTIN"]="6" ["52,VAR"]="7"
    ["52,STR"]="8" ["52,ARGS"]="70" ["53,rd"]="s 71" ["54,ld"]="s 9"
    ["54,rd"]="s 72" ["54,str"]="s 10" ["54,STMT"]="39" ["54,IF"]="3"
    ["54,FORIN"]="4" ["54,INCLUDE"]="5" ["54,BUILTIN"]="6" ["54,VAR"]="7"
    ["54,STR"]="8" ["55,rd"]="s 73" ["56,rd"]="s 74" ["57,id"]="s 18" ["57,ID"]="75"
    ["58,rd"]="s 76" ["59,rd"]="s 77" ["60,cl"]="r BOOLO BOOLO or BOOLA"
    ["60,rd"]="r BOOLO BOOLO or BOOLA" ["60,or"]="r BOOLO BOOLO or BOOLA"
    ["60,and"]="s 36" ["60,rp"]="r BOOLO BOOLO or BOOLA" ["61,ld"]="s 9"
    ["61,lp"]="s 21" ["61,str"]="s 10" ["61,STMT"]="24" ["61,IF"]="3"
    ["61,FORIN"]="4" ["61,INCLUDE"]="5" ["61,BUILTIN"]="6" ["61,VAR"]="7"
    ["61,STR"]="8" ["61,BOOLA"]="78" ["61,BOOL"]="22" ["61,ARGS"]="23"
    ["62,cl"]="r BOOLA BOOLA and BOOL" ["62,rd"]="r BOOLA BOOLA and BOOL"
    ["62,or"]="r BOOLA BOOLA and BOOL" ["62,and"]="r BOOLA BOOLA and BOOL"
    ["62,rp"]="r BOOLA BOOLA and BOOL" ["63,ld"]="s 9" ["63,lp"]="s 80"
    ["63,str"]="s 10" ["63,STMT"]="24" ["63,IF"]="3" ["63,FORIN"]="4"
    ["63,INCLUDE"]="5" ["63,BUILTIN"]="6" ["63,VAR"]="7" ["63,STR"]="8"
    ["63,BOOL"]="79" ["63,ARGS"]="23" ["64,ld"]="s 9" ["64,lp"]="s 21"
    ["64,ex"]="s 20" ["64,str"]="s 10" ["64,STMT"]="24" ["64,IF"]="3"
    ["64,FORIN"]="4" ["64,INCLUDE"]="5" ["64,BUILTIN"]="6" ["64,VAR"]="7"
    ["64,STR"]="8" ["64,BOOLS"]="81" ["64,BOOLO"]="16" ["64,UOP"]="17"
    ["64,BOOLA"]="19" ["64,BOOL"]="22" ["64,ARGS"]="23"
    ["65,cl"]="r BOOLA lp BOOLS rp" ["65,rd"]="r BOOLA lp BOOLS rp"
    ["65,or"]="r BOOLA lp BOOLS rp" ["65,and"]="r BOOLA lp BOOLS rp"
    ["65,rp"]="r BOOLA lp BOOLS rp" ["66,ld"]="s 9" ["66,cl"]="r BOOL ARGS BOP ARGS"
    ["66,rd"]="r BOOL ARGS BOP ARGS" ["66,or"]="r BOOL ARGS BOP ARGS"
    ["66,and"]="r BOOL ARGS BOP ARGS" ["66,rp"]="r BOOL ARGS BOP ARGS"
    ["66,str"]="s 10" ["66,STMT"]="39" ["66,IF"]="3" ["66,FORIN"]="4"
    ["66,INCLUDE"]="5" ["66,BUILTIN"]="6" ["66,VAR"]="7" ["66,STR"]="8"
    ["67,ld"]="s 9" ["67,rd"]="r ELIF" ["67,elif"]="r ELIF" ["67,else"]="r ELIF"
    ["67,str"]="s 10" ["67,STMT"]="2" ["67,IF"]="3" ["67,FORIN"]="4"
    ["67,INCLUDE"]="5" ["67,BUILTIN"]="6" ["67,VAR"]="7" ["67,STR"]="8"
    ["67,ELIF"]="82" ["68,ld"]="r DOC" ["68,rd"]="r DOC" ["68,str"]="r DOC"
    ["68,DOC"]="83" ["69,ld"]="r IF ld BOOLS cl DOC rd"
    ["69,cl"]="r IF ld BOOLS cl DOC rd" ["69,rd"]="r IF ld BOOLS cl DOC rd"
    ["69,elif"]="r IF ld BOOLS cl DOC rd" ["69,else"]="r IF ld BOOLS cl DOC rd"
    ["69,or"]="r IF ld BOOLS cl DOC rd" ["69,and"]="r IF ld BOOLS cl DOC rd"
    ["69,rp"]="r IF ld BOOLS cl DOC rd" ["69,ne"]="r IF ld BOOLS cl DOC rd"
    ["69,eq"]="r IF ld BOOLS cl DOC rd" ["69,gt"]="r IF ld BOOLS cl DOC rd"
    ["69,lt"]="r IF ld BOOLS cl DOC rd" ["69,ge"]="r IF ld BOOLS cl DOC rd"
    ["69,le"]="r IF ld BOOLS cl DOC rd" ["69,strgt"]="r IF ld BOOLS cl DOC rd"
    ["69,strlt"]="r IF ld BOOLS cl DOC rd" ["69,streq"]="r IF ld BOOLS cl DOC rd"
    ["69,strne"]="r IF ld BOOLS cl DOC rd" ["69,str"]="r IF ld BOOLS cl DOC rd"
    ["69,$"]="r IF ld BOOLS cl DOC rd" ["70,ld"]="s 9" ["70,cl"]="s 84"
    ["70,str"]="s 10" ["70,STMT"]="39" ["70,IF"]="3" ["70,FORIN"]="4"
    ["70,INCLUDE"]="5" ["70,BUILTIN"]="6" ["70,VAR"]="7" ["70,STR"]="8"
    ["71,ld"]="r INCLUDE ld include cl STR rd"
    ["71,cl"]="r INCLUDE ld include cl STR rd"
    ["71,rd"]="r INCLUDE ld include cl STR rd"
    ["71,elif"]="r INCLUDE ld include cl STR rd"
    ["71,else"]="r INCLUDE ld include cl STR rd"
    ["71,or"]="r INCLUDE ld include cl STR rd"
    ["71,and"]="r INCLUDE ld include cl STR rd"
    ["71,rp"]="r INCLUDE ld include cl STR rd"
    ["71,ne"]="r INCLUDE ld include cl STR rd"
    ["71,eq"]="r INCLUDE ld include cl STR rd"
    ["71,gt"]="r INCLUDE ld include cl STR rd"
    ["71,lt"]="r INCLUDE ld include cl STR rd"
    ["71,ge"]="r INCLUDE ld include cl STR rd"
    ["71,le"]="r INCLUDE ld include cl STR rd"
    ["71,strgt"]="r INCLUDE ld include cl STR rd"
    ["71,strlt"]="r INCLUDE ld include cl STR rd"
    ["71,streq"]="r INCLUDE ld include cl STR rd"
    ["71,strne"]="r INCLUDE ld include cl STR rd"
    ["71,str"]="r INCLUDE ld include cl STR rd"
    ["71,$"]="r INCLUDE ld include cl STR rd" ["72,ld"]="r BUILTIN ld ID cl ARGS rd"
    ["72,cl"]="r BUILTIN ld ID cl ARGS rd" ["72,rd"]="r BUILTIN ld ID cl ARGS rd"
    ["72,elif"]="r BUILTIN ld ID cl ARGS rd"
    ["72,else"]="r BUILTIN ld ID cl ARGS rd" ["72,or"]="r BUILTIN ld ID cl ARGS rd"
    ["72,and"]="r BUILTIN ld ID cl ARGS rd" ["72,rp"]="r BUILTIN ld ID cl ARGS rd"
    ["72,ne"]="r BUILTIN ld ID cl ARGS rd" ["72,eq"]="r BUILTIN ld ID cl ARGS rd"
    ["72,gt"]="r BUILTIN ld ID cl ARGS rd" ["72,lt"]="r BUILTIN ld ID cl ARGS rd"
    ["72,ge"]="r BUILTIN ld ID cl ARGS rd" ["72,le"]="r BUILTIN ld ID cl ARGS rd"
    ["72,strgt"]="r BUILTIN ld ID cl ARGS rd"
    ["72,strlt"]="r BUILTIN ld ID cl ARGS rd"
    ["72,streq"]="r BUILTIN ld ID cl ARGS rd"
    ["72,strne"]="r BUILTIN ld ID cl ARGS rd"
    ["72,str"]="r BUILTIN ld ID cl ARGS rd" ["72,$"]="r BUILTIN ld ID cl ARGS rd"
    ["73,ld"]="r VAR ld ID or VAR rd" ["73,cl"]="r VAR ld ID or VAR rd"
    ["73,rd"]="r VAR ld ID or VAR rd" ["73,elif"]="r VAR ld ID or VAR rd"
    ["73,else"]="r VAR ld ID or VAR rd" ["73,or"]="r VAR ld ID or VAR rd"
    ["73,and"]="r VAR ld ID or VAR rd" ["73,rp"]="r VAR ld ID or VAR rd"
    ["73,ne"]="r VAR ld ID or VAR rd" ["73,eq"]="r VAR ld ID or VAR rd"
    ["73,gt"]="r VAR ld ID or VAR rd" ["73,lt"]="r VAR ld ID or VAR rd"
    ["73,ge"]="r VAR ld ID or VAR rd" ["73,le"]="r VAR ld ID or VAR rd"
    ["73,strgt"]="r VAR ld ID or VAR rd" ["73,strlt"]="r VAR ld ID or VAR rd"
    ["73,streq"]="r VAR ld ID or VAR rd" ["73,strne"]="r VAR ld ID or VAR rd"
    ["73,str"]="r VAR ld ID or VAR rd" ["73,$"]="r VAR ld ID or VAR rd"
    ["74,ld"]="r VAR ld ID or STR rd" ["74,cl"]="r VAR ld ID or STR rd"
    ["74,rd"]="r VAR ld ID or STR rd" ["74,elif"]="r VAR ld ID or STR rd"
    ["74,else"]="r VAR ld ID or STR rd" ["74,or"]="r VAR ld ID or STR rd"
    ["74,and"]="r VAR ld ID or STR rd" ["74,rp"]="r VAR ld ID or STR rd"
    ["74,ne"]="r VAR ld ID or STR rd" ["74,eq"]="r VAR ld ID or STR rd"
    ["74,gt"]="r VAR ld ID or STR rd" ["74,lt"]="r VAR ld ID or STR rd"
    ["74,ge"]="r VAR ld ID or STR rd" ["74,le"]="r VAR ld ID or STR rd"
    ["74,strgt"]="r VAR ld ID or STR rd" ["74,strlt"]="r VAR ld ID or STR rd"
    ["74,streq"]="r VAR ld ID or STR rd" ["74,strne"]="r VAR ld ID or STR rd"
    ["74,str"]="r VAR ld ID or STR rd" ["74,$"]="r VAR ld ID or STR rd"
    ["75,rd"]="s 31" ["75,or"]="s 32" ["75,and"]="s 33"
    ["76,ld"]="r VAR ld ID and VAR rd" ["76,cl"]="r VAR ld ID and VAR rd"
    ["76,rd"]="r VAR ld ID and VAR rd" ["76,elif"]="r VAR ld ID and VAR rd"
    ["76,else"]="r VAR ld ID and VAR rd" ["76,or"]="r VAR ld ID and VAR rd"
    ["76,and"]="r VAR ld ID and VAR rd" ["76,rp"]="r VAR ld ID and VAR rd"
    ["76,ne"]="r VAR ld ID and VAR rd" ["76,eq"]="r VAR ld ID and VAR rd"
    ["76,gt"]="r VAR ld ID and VAR rd" ["76,lt"]="r VAR ld ID and VAR rd"
    ["76,ge"]="r VAR ld ID and VAR rd" ["76,le"]="r VAR ld ID and VAR rd"
    ["76,strgt"]="r VAR ld ID and VAR rd" ["76,strlt"]="r VAR ld ID and VAR rd"
    ["76,streq"]="r VAR ld ID and VAR rd" ["76,strne"]="r VAR ld ID and VAR rd"
    ["76,str"]="r VAR ld ID and VAR rd" ["76,$"]="r VAR ld ID and VAR rd"
    ["77,ld"]="r VAR ld ID and STR rd" ["77,cl"]="r VAR ld ID and STR rd"
    ["77,rd"]="r VAR ld ID and STR rd" ["77,elif"]="r VAR ld ID and STR rd"
    ["77,else"]="r VAR ld ID and STR rd" ["77,or"]="r VAR ld ID and STR rd"
    ["77,and"]="r VAR ld ID and STR rd" ["77,rp"]="r VAR ld ID and STR rd"
    ["77,ne"]="r VAR ld ID and STR rd" ["77,eq"]="r VAR ld ID and STR rd"
    ["77,gt"]="r VAR ld ID and STR rd" ["77,lt"]="r VAR ld ID and STR rd"
    ["77,ge"]="r VAR ld ID and STR rd" ["77,le"]="r VAR ld ID and STR rd"
    ["77,strgt"]="r VAR ld ID and STR rd" ["77,strlt"]="r VAR ld ID and STR rd"
    ["77,streq"]="r VAR ld ID and STR rd" ["77,strne"]="r VAR ld ID and STR rd"
    ["77,str"]="r VAR ld ID and STR rd" ["77,$"]="r VAR ld ID and STR rd"
    ["78,cl"]="r BOOLO BOOLO or UOP BOOLA" ["78,rd"]="r BOOLO BOOLO or UOP BOOLA"
    ["78,or"]="r BOOLO BOOLO or UOP BOOLA" ["78,and"]="s 36"
    ["78,rp"]="r BOOLO BOOLO or UOP BOOLA" ["79,cl"]="r BOOLA BOOLA and UOP BOOL"
    ["79,rd"]="r BOOLA BOOLA and UOP BOOL" ["79,or"]="r BOOLA BOOLA and UOP BOOL"
    ["79,and"]="r BOOLA BOOLA and UOP BOOL" ["79,rp"]="r BOOLA BOOLA and UOP BOOL"
    ["80,ld"]="s 9" ["80,lp"]="s 21" ["80,ex"]="s 20" ["80,str"]="s 10"
    ["80,STMT"]="24" ["80,IF"]="3" ["80,FORIN"]="4" ["80,INCLUDE"]="5"
    ["80,BUILTIN"]="6" ["80,VAR"]="7" ["80,STR"]="8" ["80,BOOLS"]="85"
    ["80,BOOLO"]="16" ["80,UOP"]="17" ["80,BOOLA"]="19" ["80,BOOL"]="22"
    ["80,ARGS"]="23" ["81,rp"]="s 86" ["82,rd"]="r ELSE" ["82,elif"]="s 88"
    ["82,else"]="s 89" ["82,ELSE"]="87" ["83,ld"]="s 9" ["83,rd"]="s 90"
    ["83,str"]="s 10" ["83,STMT"]="2" ["83,IF"]="3" ["83,FORIN"]="4"
    ["83,INCLUDE"]="5" ["83,BUILTIN"]="6" ["83,VAR"]="7" ["83,STR"]="8"
    ["84,ld"]="r DOC" ["84,rd"]="r DOC" ["84,str"]="r DOC" ["84,DOC"]="91"
    ["85,rp"]="s 92" ["86,cl"]="r BOOLA BOOLA and lp BOOLS rp"
    ["86,rd"]="r BOOLA BOOLA and lp BOOLS rp"
    ["86,or"]="r BOOLA BOOLA and lp BOOLS rp"
    ["86,and"]="r BOOLA BOOLA and lp BOOLS rp"
    ["86,rp"]="r BOOLA BOOLA and lp BOOLS rp" ["87,rd"]="s 93" ["88,ld"]="s 9"
    ["88,lp"]="s 21" ["88,ex"]="s 20" ["88,str"]="s 10" ["88,STMT"]="24"
    ["88,IF"]="3" ["88,FORIN"]="4" ["88,INCLUDE"]="5" ["88,BUILTIN"]="6"
    ["88,VAR"]="7" ["88,STR"]="8" ["88,BOOLS"]="94" ["88,BOOLO"]="16"
    ["88,UOP"]="17" ["88,BOOLA"]="19" ["88,BOOL"]="22" ["88,ARGS"]="23"
    ["89,cl"]="s 95" ["90,ld"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,cl"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,rd"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,elif"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,else"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,or"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,and"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,rp"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,ne"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,eq"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,gt"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,lt"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,ge"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,le"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,strgt"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,strlt"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,streq"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,strne"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,str"]="r IF ld BOOLS cl DOC cl DOC rd"
    ["90,$"]="r IF ld BOOLS cl DOC cl DOC rd" ["91,ld"]="s 9" ["91,rd"]="s 96"
    ["91,str"]="s 10" ["91,STMT"]="2" ["91,IF"]="3" ["91,FORIN"]="4"
    ["91,INCLUDE"]="5" ["91,BUILTIN"]="6" ["91,VAR"]="7" ["91,STR"]="8"
    ["92,cl"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["92,rd"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["92,or"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["92,and"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["92,rp"]="r BOOLA BOOLA and UOP lp BOOLS rp"
    ["93,ld"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,cl"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,rd"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,elif"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,else"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,or"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,and"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,rp"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,ne"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,eq"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,gt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,lt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,ge"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,le"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,strgt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,strlt"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,streq"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,strne"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,str"]="r IF ld if BOOLS cl DOC ELIF ELSE rd"
    ["93,$"]="r IF ld if BOOLS cl DOC ELIF ELSE rd" ["94,cl"]="s 97"
    ["95,ld"]="r DOC" ["95,rd"]="r DOC" ["95,str"]="r DOC" ["95,DOC"]="98"
    ["96,ld"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,cl"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,rd"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,elif"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,else"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,or"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,and"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,rp"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,ne"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,eq"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,gt"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,lt"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,ge"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,le"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,strgt"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,strlt"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,streq"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,strne"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,str"]="r FORIN ld for ID in ARGS cl DOC rd"
    ["96,$"]="r FORIN ld for ID in ARGS cl DOC rd" ["97,ld"]="r DOC"
    ["97,rd"]="r DOC" ["97,elif"]="r DOC" ["97,else"]="r DOC" ["97,str"]="r DOC"
    ["97,DOC"]="99" ["98,ld"]="s 9" ["98,rd"]="r ELSE else cl DOC" ["98,str"]="s 10"
    ["98,STMT"]="2" ["98,IF"]="3" ["98,FORIN"]="4" ["98,INCLUDE"]="5"
    ["98,BUILTIN"]="6" ["98,VAR"]="7" ["98,STR"]="8" ["99,ld"]="s 9"
    ["99,rd"]="r ELIF ELIF elif BOOLS cl DOC"
    ["99,elif"]="r ELIF ELIF elif BOOLS cl DOC"
    ["99,else"]="r ELIF ELIF elif BOOLS cl DOC" ["99,str"]="s 10" ["99,STMT"]="2"
    ["99,IF"]="3" ["99,FORIN"]="4" ["99,INCLUDE"]="5" ["99,BUILTIN"]="6"
    ["99,VAR"]="7" ["99,STR"]="8"
) # }}}
# <<< BPT_PARSE_TABLE_E <<<

return 0 2>/dev/null || bpt.main "$@"
