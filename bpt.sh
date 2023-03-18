#!/bin/bash
# shellcheck disable=SC2317

source private/states.sh

# Import once
if [[ -n $__DEFINED_BPT_SH ]]; then return; fi
readonly __DEFINED_BPT_SH=1

# The shift-reduce LR(1) parser.
#
# $1: Parse table name.
#   The parse table should be an associative array where the key is
#   <state>,<token> and the value actions (s <k>/r <rule>/a/<k>).
# $2: Reduce function hook
#   Args passed to this function:
#     $1: The reduce rule (as a string). i.e. "LHS RHS1 RHS2 ..."
#     ${@:1}: RHS contents.
#   The reduce function should store the reduce result to the `result` variable.
# $3: Error handler function hook
#   Args passed to this function:
#     $1: Line
#     $2: Column
#     $3: Default error message
# $4: (optional) If set, enable debug.
shlr.parse() (
    local -rn table="$1"
    local -r reduce_fn="${2:-echo}"
    local -r error_fn="${3:-__error}"
    if [[ -n $4 ]]; then local -r NDEBUG=false; else local -r NDEBUG=true; fi

    # 99 should be enough...
    # I assume no one's writing a BNF with 99 RHSs ...
    local -a STATE_PTRN=('')
    for i in {1..99}; do STATE_PTRN[i]="${STATE_PTRN[i - 1]}:*"; done

    # Parse stack and associatied content stack
    # Using string manipulation for states is faster than using an array.
    local -a states=':0' contents=('')
    # Look-ahead token and its content
    local token='' content=''
    # Location tracking variables
    local num_lines=0 num_bytes=0

    # $1: Goto state after shift
    __shift() {
        $NDEBUG ||
            echo "[DBG] ${states##*:} Shift $1 \`$content\`" >&2
        states+=":$1"
        contents+=("$content")
        token='' content=''
    }

    # $1: Rule
    __reduce() {
        $NDEBUG ||
            echo "[DBG] ${states##*:} Reduce $1" >&2

        # shellcheck disable=SC2206
        # Although not robust, word splitting is faster than
        #   read -ra rule <<<"$1"
        local -a rule=($1)
        local num_rhs=$((${#rule[@]} - 1))

        # Reduce and goto state

        # shellcheck disable=SC2295
        states="${states%${STATE_PTRN[$num_rhs]}}"
        states+=":${table["${states##*:},${rule[0]}"]}"

        # Run reduce hook, discard reduced contents, and save the reduce result
        local -n result="contents[$((${#contents[@]} - num_rhs))]"
        $reduce_fn "$1" "${contents[@]:${#contents[@]}-$num_rhs}" || exit 1

        # The following is faster than:
        #   contents=("${contents[@]:0:${#contents[@]}-$num_rhs}")
        # since it avoides copy large strings.
        local i=0 b=$((${#contents[@]} - 1)) e=$((${#contents[@]} - num_rhs + 1))
        for ((i = b; i >= e; --i)); do unset "contents[$i]"; done
    }

    # Simply print the result
    __accept() {
        $NDEBUG || echo "[DBG] Result accepted" >&2
        printf '%s' "${contents[@]}"
    }

    # Default error handler
    __error() {
        echo "Error: Line $(($1 + 1)) Column $(($2 + 1))"
        echo "$3"
    } >&2

    while true; do
        [[ -n $token ]] || {
            read -r token num_lines num_bytes
            local OIFS="$IFS" && IFS=''
            read -r content
            IFS="$OIFS"
        }

        action="${table["${states##*:},$token"]}"
        case "$action" in
        s*) __shift "${action#s }" ;;  # Shift
        r*) __reduce "${action#r }" ;; # Reduce
        a) __accept && break ;;        # Accept
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
        *) # Parse Table Error
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
#
# Terminal token name to content mappings:
#   str: Anything outside the toplevel `ld ... rd` or
#        Anything inside `"..."` or `'...'` within any `ld ... rd`
#     Note1: `"` inside `"..."` needs to be escaped using `\"`,
#            and the same for `'` inside `'...'`.
#     Note2: str cannot contain newline since both the input and the output
#            of the scanner is line-based. Newline is expressed by the nl token.
#   nl: \n
#   ld: ${ldelim}   rd: ${rdelim}       lp: (           rp: )
#   cl: :           ex: !               eq: -eq         ne: -ne
#   lt: -lt         gt: -gt             le: -le         ge: -ge
#   streq: ==       strne: !=           strlt: <        strgt: >
#   and|or|if|elif|else|for|in|include: <as is>
#   id: [[:alpha:]_][[:alnum:]_]*
bpt.scan() {
    local -r ld="$1" rd="$2"
    local -r e_ld="$(printf '%q' "$ld")" e_rd="$(printf '%q' "$rd")"
    bpt.__test_delims "$ld" "$rd" || return 1

    # Keywords
    local -ra KW=(
        "${e_ld}" "${e_rd}"
        '-eq' '-ne' '-gt' '-lt' '-ge' '-le'
        '==' '!=' '>' '<' ':' '\!' '\"' "\\'" '\(' '\)'
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

    # Tokenizer
    local line='' content=''
    while read -r line; do
        # Only count newlines outside `ld ... rd` and inside quotes.
        if [[ $num_lines -gt 0 && ($num_ld -eq 0 || -n $quote) ]]; then
            echo nl
            echo ''
        fi
        # Scan the line
        while [[ -n "$line" ]]; do
            if [[ $num_ld -eq 0 ]]; then
                # Outside `ld ... rd`
                if [[ $line =~ ^(${e_ld}) ]]; then
                    ((++num_ld))
                    content="${BASH_REMATCH[1]}"
                    echo -n ld
                elif [[ $line =~ (${e_ld}) ]]; then
                    content="${line%%"${BASH_REMATCH[1]}"*}"
                    echo -n str
                else
                    content="$line"
                    echo -n str
                fi
                echo " $num_lines $num_bytes"
                echo "$content"
            elif [[ -n $quote ]]; then
                # Inside quotes in `ld ... rd`
                local string=''
                if [[ $line =~ "\\"${quote} ]]; then
                    # Escape quote inside string
                    string="${line%%"\\${quote}"*}${quote}"
                    content="${line%%"\\${quote}"*}""\\${quote}"
                elif [[ $line =~ ${quote} ]]; then
                    # Ending quote
                    string="${line%%"${quote}"*}"
                    # Remove the closing " from the contnet
                    content="$string"'"'
                    quote=''
                else
                    content="$line"
                fi
                [[ -z "$string" ]] || {
                    echo "str $num_lines $num_bytes"
                    echo "$string"
                }
            elif [[ $line =~ ^(${KW_RE}) ]]; then
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
                '"' | "'") quote="$content" ;;
                "$ld")
                    ((++num_ld))
                    echo -n ld
                    ;;
                "$rd")
                    [[ -n $num_ld ]] || {
                        echo "Extra '$rd'."
                        return 1
                    }
                    ((--num_ld))
                    echo -n rd
                    ;;
                and | or | if | elif | else) ;&
                for | in | include) echo -n "$content" ;;
                *)
                    echo "Internal error: Unrecognized token ${content}" >&2
                    return 1
                    ;;
                esac
                [[ -n $quote ]] || {
                    echo " $num_lines $num_bytes"
                    echo "$content"
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
                    echo "$content is not a valid identifier" >&2
                    return 1
                fi
                content="${BASH_REMATCH[1]}"
                echo "id $num_lines $num_bytes"
                echo "$content"
            fi
            line="${line#"$content"}"
            ((num_bytes += ${#content}))
        done
        ((++num_lines))
        ((num_bytes = 0))
    done
    echo "$ $num_lines 0" # The EOF token
    echo ''               # The EOF content (empty)
}

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
bpt.__reduce_collect_vars() {
    # Note: When using position argument ($@), index is 1-based.
    # shellcheck disable=SC2206
    local -a rule=($1)
    shift

    case "${rule[0]}" in
    NL | UOP | BOP) result='' ;;
    VAR)
        result="$2"$'\n'
        [[ ${#rule[@]} -eq 4 || ${rule[4]} != VAR ]] || result+="$4"
        ;;
    BUILTIN) result="$4" ;;
    INCLUDE) result="$(__recursive_process "$4")"$'\n' ;;
    FORIN)
        result=''
        local var
        while read -r var; do
            [[ -z $var || $var == "$3" ]] || result+="$var"$'\n'
        done <<<"$7"
        ;;
    BOOL)
        case "${#rule[@]}" in
        3) result="$2" ;;
        4) result="$1$3" ;;
        esac
        ;;
    BOOLA)
        case "${#rule[@]}" in
        4) case "${rule[1]}" in
            lp) result="$2" ;;
            BOOLA) result="$1$3" ;;
            esac ;;
        6) result="$1$4" ;;
        esac
        ;;
    BOOLO) [[ "${#rule}" -eq 2 ]] || result+="$3" ;;
    ELSE) result="$3" ;;
    ELIF) result="$3$5" ;;
    IF) result="$3$5$6$7" ;;
    STMT) case "${rule[1]}" in STR) result='' ;; *) result="$1" ;; esac ;;
    *) local OIFS="$IFS" && IFS='' && result="$*" && IFS="$OIFS" ;;
    esac
}

bpt.__reduce_collect_includes() {
    # shellcheck disable=SC2206
    local -a rule=($1)
    shift

    case "${rule[0]}" in
    STR) result="$1" ;;
    INCLUDE) result="$4"$'\n' && result+="$(__recursive_process "$4")" ;;
    STMT) case "${rule[1]}" in INCLUDE) result="$1" ;; *) result='' ;; esac ;;
    *) local OIFS="$IFS" && IFS='' && result="$*" && IFS="$OIFS" ;;
    esac
}

# The reduce function to collect all top-level strings, discarding all else.
bpt.__reduce_collect_toplevel_strings() {
    # shellcheck disable=SC2206
    local -a rule=($1)
    shift

    case "${rule[0]}" in
    STR) result="$1" ;;
    NL) result=$'\n' ;;
    STMT) case "${rule[1]}" in STR) result="$1"$'\n' ;; *) result='' ;; esac ;;
    *) local OIFS="$IFS" && IFS='' && result="$*" && IFS="$OIFS" ;;
    esac
}

# The reduce function to generate the template
bpt.__reduce_generate() {
    # shellcheck disable=SC2206
    local -a rule=($1)
    shift

    case "${rule[0]}" in
    NL) result=$'\n' ;;
    STR | ID | UOP | BOP) result="$1" ;;
    VAR)
        case "${rule[3]}" in
        rd) result="\${$2}" ;;
        or) result="\${$2:-" ;;&
        and) result="\${$2:+" ;;&
        *) case "${rule[4]}" in
            VAR) result+="\"$4\"}" ;;
            STR) result+="${4@Q}}" ;;
            esac ;;
        esac
        ;;
    ARGS)
        [[ ${#rule[@]} -ne 1 ]] || {
            result=''
            return
        }
        # Strip the tag from STMT
        local stmt_type="${2%%:*}"
        local stmt="${2#*:}"

        # Note 1: Since `result` is a nameref to `contents[#contents-#rhs]` and
        #   the latter is passed in exactly as $1, the `result="$1"` command
        #   is unnecessary here. Removing it improves performance.
        # Note 2: Use `${stmt@Q}` is faster than `printf '%q' ${stmt}`
        case "$stmt_type" in
        STR) result+=" ${stmt@Q} " ;;
        VAR | BUILTIN) result+=" $stmt " ;;
        INCLUDE | FORIN | IF) result+=" \"\$($stmt)\" " ;;
        esac
        ;;
    BUILTIN)
        # filter allowed builtints
        case "$2" in
        toupper | tolower | len | seq | quote) ;;
        *) echo "Unrecognized builtin function $2" >&2 && exit 1 ;;
        esac
        result="\$($2 $4)"
        ;;
    INCLUDE) result="$(__recursive_process "$4")" ;;
    FORIN) result="for $3 in $5; do $7 done" ;;
    BOOL)
        case "${#rule[@]}" in
        3) result="$*" ;; # UOP BOOL
        *)
            # Don't use the name contents (avoid nameref collision)
            local -a rhss=()
            # Strip the tag from STMT
            local stmt_l_type="${1%%:*}"
            local stmt_l="${1#*:}"
            case "$stmt_l_type" in
            ID | STR) rhss[0]="\$(e ${stmt_l@Q})" ;;
            VAR) rhss[0]="\"$stmt_l\"" ;;
            *) rhss[0]="\$(${stmt_l})" ;;
            esac
            # If rule is STMT op STMT, deal with op and RHS
            [[ ${#rule[@]} -eq 2 ]] || {
                rhss[1]="$2"
                local stmt_r_type="${3%%:*}"
                local stmt_r="${3#*:}"
                case "$stmt_r_type" in
                ID | STR) rhss[2]="\$(e ${stmt_r@Q})" ;;
                VAR) rhss[2]="\"$stmt_r\"" ;;
                *) rhss[2]="\$(${stmt_r})" ;;
                esac
            }
            result="${rhss[*]}"
            ;;
        esac
        ;;
    BOOLA) # For BOOLA->BOOL (case 2), result is already $1, thus no-op.
        case "${#rule[@]}" in
        4) case "${rule[1]}" in
            BOOLA) result="$1 && $3" ;;
            lp) result="( $2 )" ;;
            esac ;;
        6) result="$1 && ( $4 )" ;;
        esac
        ;;
    BOOLO) [[ ${#rule[@]} -eq 2 ]] || result="$1 || $3" ;;
    ELSE)
        case "${#rule[@]}" in
        1) result='' ;;
        *) result="else $3" ;;
        esac
        ;;
    ELIF)
        case "${#rule[@]}" in
        1) result='' ;;
        *) result="$1; elif [[ $3 ]]; then $5" ;;
        esac
        ;;
    IF) result="if [[ $3 ]]; then ${*:5:3} fi" ;;
    STMT)
        # Tag the sub-type to the reduce result
        # (Need to strip the tag wherever STMT is used)
        result="${rule[1]}:$1"
        ;;
    DOC) # Similar to ARGS but produces commands instead of strings
        # Return when document is empty
        [[ ${#rule[@]} -ne 1 ]] || {
            result=''
            return
        }

        # Strip the tag from STMT
        local stmt_type="${2%%:*}"
        local stmt="${2#*:}"

        # Reduce the document
        case "$stmt_type" in
        STR) result+="{ e ${stmt@Q}; };" ;;
        VAR) result+="{ e \"$stmt\"; };" ;;
        INCLUDE) result+="$stmt" ;;
        FORIN | IF) result+="{ $stmt; };" ;;
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
#   STR     -> str | NL .
#   NL      -> nl .
#
# Note1: the combination of BOOLO and BOOLA is equivalent to the expression
#   grammar `BOOLS -> BOOL or BOOL | BOOL and BOOL | lp BOOL rp | BOOL .`
#   with left associativity for `and` and `or` meanwhile `and` having higher
#   precidence than `or`. (Solves shift-reduce conflict with subclassing).
# See: https://ece.uwaterloo.ca/~vganesh/TEACHING/W2014/lectures/lecture08.pdf
#
# Note2: the `ID -> id`, `STR -> str | NL`, and `NL -> nl` rules are not
#   redundant. They are for better controls when hooking the reduce function.
bpt.process() (
    local -r ld="$1" rd="$2" file="$4" debug="$5"
    local -r reduce_fn="${3:-bpt.__reduce_generate}"
    local -a file_stack=("${file_stack[@]}")

    [[ -f $file ]] || {
        echo "Error: file '$file' does not exist"
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
    shlr.parse PARSE_TABLE "$reduce_fn" __error_handler "$debug" < <(bpt.scan "$ld" "$rd" <"$file")
)

bpt.print_help() {
    :
}

bpt.main() {
    local ld='{{' rd='}}'
    local infile=''
    local reduce_fn=bpt.__reduce_generate
    local post_process=eval
    local scan=false
    local debug=''

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
        -l | --left-delimiter) shift && ld="$1" ;;
        -r | --right-delimiter) shift && rd="$1" ;;
        -s | --scan) scan=true ;;
        -ge | --generate-eval)
            reduce_fn=bpt.__reduce_collect_vars
            post_process='eval'
            ;;
        -cv | --collect-vars)
            reduce_fn=bpt.__reduce_collect_vars
            post_process=bpt.__dedup
            ;;
        -ci | --collect-includes)
            reduce_fn=bpt.__reduce_collect_includes
            post_process=bpt.__dedup
            ;;
        -cs | --collect-strings)
            reduce_fn=bpt.__reduce_collect_toplevel_strings
            post_process='echo'
            ;;
        -g | --generate)
            reduce_fn=bpt.__reduce_generate
            post_process='echo'
            ;;
        -d | --debug) debug=1 ;;
        *)
            [[ -z $infile ]] || {
                bpt.print_help
                return 1
            }
            infile="$1"
            ;;
        esac
        shift
    done

    # Global constants for pretty-printing
    local -rA BPT_PP_TOKEN_TABLE=(
        [ld]="$ld" [rd]="$rd"
        [eq]='-eq' [ne]='-ne' [gt]='-gt' [lt]='-lt' [ge]='-ge' [le]='-le'
        [streq]='==' [strne]='!=' [strgt]='>' [strlt]='<'
        [cl]=':' [ex]='!' [lp]='(' [rp]=')' [nl]='\n')

    # Deduplication function for collect-{var,include}
    bpt.__dedup() { echo "$1" | sort | uniq; }

    # Append this if reducer is bpt.__reduce_generate
    local HEADER=''
    [[ $reduce_fn != bpt.__reduce_generate ]] || {
        read -r -d '' HEADER <<-'EOF'
			#!/bin/bash
			e(){ echo -n "$@"; };
		EOF
    }

    if $scan; then
        bpt.scan "$ld" "$rd" <"$infile"
    else
        # Process templates
        result="$(bpt.process "$ld" "$rd" "$reduce_fn" "$infile" "$debug")" &&
            $post_process "$HEADER$result"
    fi
}

(return 0 2>/dev/null) || bpt.main "$@"
