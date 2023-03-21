#!/bin/bash
# vim: set foldlevel=0:
# shellcheck disable=SC2317

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
bpt.__lr_parse() (
    local -rn table="$1"
    local -r reduce_fn="${2:-echo}"
    local -r error_fn="${3:-__error}"
    if [[ -n $4 ]]; then local -r NDEBUG=false; else local -r NDEBUG=true; fi

    # 99 should be enough ...
    # I assume no one's writing a BNF with 99 RHSs ...
    local -a STATE_PTRN=('')
    for i in {1..99}; do STATE_PTRN[i]="${STATE_PTRN[i - 1]}:*"; done

    # Parse stack and associatied content stack
    # Using string manipulation for states is faster than using an array.
    local states=':0' stack_size=1
    # Large dict indexing is significantly faster than a regular one.
    # Thus we use stack_size + associative array to emulate a regular array.
    local -A contents=([0]='')
    # Look-ahead token and its content
    local token='' content=''
    # Location tracking variables
    local num_lines=0 num_bytes=0

    # $1: Goto state after shift
    __shift() {
        $NDEBUG ||
            echo "[DBG] ${states##*:} Shift $1 \`$content\`" >&2
        states+=":$1"
        contents["$stack_size"]="$content"
        ((++stack_size))
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
        local -n result="contents[$((stack_size - num_rhs))]"
        local i=0 b=$((stack_size - num_rhs)) e=$((stack_size))
        local args=''
        for ((i = b; i < e; ++i)); do args+=" \"\${contents[$i]}\""; done

        eval "$reduce_fn ${1@Q} $args" || exit 1

        local i=0 b=$((stack_size - 1)) e=$((stack_size - num_rhs + 1))
        for ((i = b; i >= e; --i)); do unset "contents[$i]"; done
        stack_size="$e"
    }

    # Simply print the result
    __accept() {
        $NDEBUG || echo "[DBG] Result accepted" >&2
        printf '%s' "${contents[1]}"
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
        or) result="\${$2:-\$(e " ;;&
        and) result="\${$2:+\$(e " ;;&
        *) case "${rule[4]}" in
            VAR) result+="\"$4\")}" ;;
            STR) result+="${4@Q})}" ;;
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
        len | seq) result="\$($2 $4)" ;;
        quote) result="\"$4\"" ;;
        *) echo "Unrecognized builtin function $2" >&2 && exit 1 ;;
        esac
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
        BUILTIN | VAR) result+="{ e \"$stmt\"; };" ;;
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
    bpt.__lr_parse BPT_PARSE_TABLE "$reduce_fn" __error_handler "$debug" < <(bpt.scan "$ld" "$rd" <"$file")
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

    # >>> BPT_PARSE_TABLE_S >>>
    local -rA BPT_PARSE_TABLE=( # {{{
        ["0,ld"]="r DOC" ["0,str"]="r DOC" ["0,nl"]="r DOC" ["0,$"]="r DOC"
        ["0,DOC"]="1" ["1,ld"]="s 9" ["1,str"]="s 10" ["1,nl"]="s 12" ["1,$"]="a"
        ["1,STMT"]="2" ["1,IF"]="3" ["1,FORIN"]="4" ["1,INCLUDE"]="5" ["1,BUILTIN"]="6"
        ["1,VAR"]="7" ["1,STR"]="8" ["1,NL"]="11" ["2,ld"]="r DOC DOC STMT"
        ["2,rd"]="r DOC DOC STMT" ["2,elif"]="r DOC DOC STMT"
        ["2,else"]="r DOC DOC STMT" ["2,str"]="r DOC DOC STMT" ["2,nl"]="r DOC DOC STMT"
        ["2,$"]="r DOC DOC STMT" ["3,ld"]="r STMT IF" ["3,cl"]="r STMT IF"
        ["3,rd"]="r STMT IF" ["3,elif"]="r STMT IF" ["3,else"]="r STMT IF"
        ["3,or"]="r STMT IF" ["3,and"]="r STMT IF" ["3,rp"]="r STMT IF"
        ["3,ne"]="r STMT IF" ["3,eq"]="r STMT IF" ["3,gt"]="r STMT IF"
        ["3,lt"]="r STMT IF" ["3,ge"]="r STMT IF" ["3,le"]="r STMT IF"
        ["3,strgt"]="r STMT IF" ["3,strlt"]="r STMT IF" ["3,streq"]="r STMT IF"
        ["3,strne"]="r STMT IF" ["3,str"]="r STMT IF" ["3,nl"]="r STMT IF"
        ["3,$"]="r STMT IF" ["4,ld"]="r STMT FORIN" ["4,cl"]="r STMT FORIN"
        ["4,rd"]="r STMT FORIN" ["4,elif"]="r STMT FORIN" ["4,else"]="r STMT FORIN"
        ["4,or"]="r STMT FORIN" ["4,and"]="r STMT FORIN" ["4,rp"]="r STMT FORIN"
        ["4,ne"]="r STMT FORIN" ["4,eq"]="r STMT FORIN" ["4,gt"]="r STMT FORIN"
        ["4,lt"]="r STMT FORIN" ["4,ge"]="r STMT FORIN" ["4,le"]="r STMT FORIN"
        ["4,strgt"]="r STMT FORIN" ["4,strlt"]="r STMT FORIN" ["4,streq"]="r STMT FORIN"
        ["4,strne"]="r STMT FORIN" ["4,str"]="r STMT FORIN" ["4,nl"]="r STMT FORIN"
        ["4,$"]="r STMT FORIN" ["5,ld"]="r STMT INCLUDE" ["5,cl"]="r STMT INCLUDE"
        ["5,rd"]="r STMT INCLUDE" ["5,elif"]="r STMT INCLUDE"
        ["5,else"]="r STMT INCLUDE" ["5,or"]="r STMT INCLUDE" ["5,and"]="r STMT INCLUDE"
        ["5,rp"]="r STMT INCLUDE" ["5,ne"]="r STMT INCLUDE" ["5,eq"]="r STMT INCLUDE"
        ["5,gt"]="r STMT INCLUDE" ["5,lt"]="r STMT INCLUDE" ["5,ge"]="r STMT INCLUDE"
        ["5,le"]="r STMT INCLUDE" ["5,strgt"]="r STMT INCLUDE"
        ["5,strlt"]="r STMT INCLUDE" ["5,streq"]="r STMT INCLUDE"
        ["5,strne"]="r STMT INCLUDE" ["5,str"]="r STMT INCLUDE"
        ["5,nl"]="r STMT INCLUDE" ["5,$"]="r STMT INCLUDE" ["6,ld"]="r STMT BUILTIN"
        ["6,cl"]="r STMT BUILTIN" ["6,rd"]="r STMT BUILTIN" ["6,elif"]="r STMT BUILTIN"
        ["6,else"]="r STMT BUILTIN" ["6,or"]="r STMT BUILTIN" ["6,and"]="r STMT BUILTIN"
        ["6,rp"]="r STMT BUILTIN" ["6,ne"]="r STMT BUILTIN" ["6,eq"]="r STMT BUILTIN"
        ["6,gt"]="r STMT BUILTIN" ["6,lt"]="r STMT BUILTIN" ["6,ge"]="r STMT BUILTIN"
        ["6,le"]="r STMT BUILTIN" ["6,strgt"]="r STMT BUILTIN"
        ["6,strlt"]="r STMT BUILTIN" ["6,streq"]="r STMT BUILTIN"
        ["6,strne"]="r STMT BUILTIN" ["6,str"]="r STMT BUILTIN"
        ["6,nl"]="r STMT BUILTIN" ["6,$"]="r STMT BUILTIN" ["7,ld"]="r STMT VAR"
        ["7,cl"]="r STMT VAR" ["7,rd"]="r STMT VAR" ["7,elif"]="r STMT VAR"
        ["7,else"]="r STMT VAR" ["7,or"]="r STMT VAR" ["7,and"]="r STMT VAR"
        ["7,rp"]="r STMT VAR" ["7,ne"]="r STMT VAR" ["7,eq"]="r STMT VAR"
        ["7,gt"]="r STMT VAR" ["7,lt"]="r STMT VAR" ["7,ge"]="r STMT VAR"
        ["7,le"]="r STMT VAR" ["7,strgt"]="r STMT VAR" ["7,strlt"]="r STMT VAR"
        ["7,streq"]="r STMT VAR" ["7,strne"]="r STMT VAR" ["7,str"]="r STMT VAR"
        ["7,nl"]="r STMT VAR" ["7,$"]="r STMT VAR" ["8,ld"]="r STMT STR"
        ["8,cl"]="r STMT STR" ["8,rd"]="r STMT STR" ["8,elif"]="r STMT STR"
        ["8,else"]="r STMT STR" ["8,or"]="r STMT STR" ["8,and"]="r STMT STR"
        ["8,rp"]="r STMT STR" ["8,ne"]="r STMT STR" ["8,eq"]="r STMT STR"
        ["8,gt"]="r STMT STR" ["8,lt"]="r STMT STR" ["8,ge"]="r STMT STR"
        ["8,le"]="r STMT STR" ["8,strgt"]="r STMT STR" ["8,strlt"]="r STMT STR"
        ["8,streq"]="r STMT STR" ["8,strne"]="r STMT STR" ["8,str"]="r STMT STR"
        ["8,nl"]="r STMT STR" ["8,$"]="r STMT STR" ["9,if"]="s 13" ["9,for"]="s 14"
        ["9,include"]="s 15" ["9,id"]="s 17" ["9,ID"]="16" ["10,ld"]="r STR str"
        ["10,cl"]="r STR str" ["10,rd"]="r STR str" ["10,elif"]="r STR str"
        ["10,else"]="r STR str" ["10,or"]="r STR str" ["10,and"]="r STR str"
        ["10,rp"]="r STR str" ["10,ne"]="r STR str" ["10,eq"]="r STR str"
        ["10,gt"]="r STR str" ["10,lt"]="r STR str" ["10,ge"]="r STR str"
        ["10,le"]="r STR str" ["10,strgt"]="r STR str" ["10,strlt"]="r STR str"
        ["10,streq"]="r STR str" ["10,strne"]="r STR str" ["10,str"]="r STR str"
        ["10,nl"]="r STR str" ["10,$"]="r STR str" ["11,ld"]="r STR NL"
        ["11,cl"]="r STR NL" ["11,rd"]="r STR NL" ["11,elif"]="r STR NL"
        ["11,else"]="r STR NL" ["11,or"]="r STR NL" ["11,and"]="r STR NL"
        ["11,rp"]="r STR NL" ["11,ne"]="r STR NL" ["11,eq"]="r STR NL"
        ["11,gt"]="r STR NL" ["11,lt"]="r STR NL" ["11,ge"]="r STR NL"
        ["11,le"]="r STR NL" ["11,strgt"]="r STR NL" ["11,strlt"]="r STR NL"
        ["11,streq"]="r STR NL" ["11,strne"]="r STR NL" ["11,str"]="r STR NL"
        ["11,nl"]="r STR NL" ["11,$"]="r STR NL" ["12,ld"]="r NL nl" ["12,cl"]="r NL nl"
        ["12,rd"]="r NL nl" ["12,elif"]="r NL nl" ["12,else"]="r NL nl"
        ["12,or"]="r NL nl" ["12,and"]="r NL nl" ["12,rp"]="r NL nl" ["12,ne"]="r NL nl"
        ["12,eq"]="r NL nl" ["12,gt"]="r NL nl" ["12,lt"]="r NL nl" ["12,ge"]="r NL nl"
        ["12,le"]="r NL nl" ["12,strgt"]="r NL nl" ["12,strlt"]="r NL nl"
        ["12,streq"]="r NL nl" ["12,strne"]="r NL nl" ["12,str"]="r NL nl"
        ["12,nl"]="r NL nl" ["12,$"]="r NL nl" ["13,ld"]="s 9" ["13,lp"]="s 20"
        ["13,ex"]="s 24" ["13,str"]="s 10" ["13,nl"]="s 12" ["13,STMT"]="22"
        ["13,IF"]="3" ["13,FORIN"]="4" ["13,INCLUDE"]="5" ["13,BUILTIN"]="6"
        ["13,VAR"]="7" ["13,STR"]="8" ["13,BOOLO"]="18" ["13,BOOLA"]="19"
        ["13,BOOL"]="21" ["13,UOP"]="23" ["13,NL"]="11" ["14,id"]="s 17" ["14,ID"]="25"
        ["15,cl"]="s 26" ["16,cl"]="s 27" ["16,rd"]="s 28" ["16,or"]="s 29"
        ["16,and"]="s 30" ["17,cl"]="r ID id" ["17,rd"]="r ID id" ["17,or"]="r ID id"
        ["17,and"]="r ID id" ["17,in"]="r ID id" ["18,cl"]="s 31" ["18,or"]="s 32"
        ["19,cl"]="r BOOLO BOOLA" ["19,or"]="r BOOLO BOOLA" ["19,and"]="s 33"
        ["19,rp"]="r BOOLO BOOLA" ["20,ld"]="s 9" ["20,lp"]="s 20" ["20,ex"]="s 24"
        ["20,str"]="s 10" ["20,nl"]="s 12" ["20,STMT"]="22" ["20,IF"]="3"
        ["20,FORIN"]="4" ["20,INCLUDE"]="5" ["20,BUILTIN"]="6" ["20,VAR"]="7"
        ["20,STR"]="8" ["20,BOOLO"]="34" ["20,BOOLA"]="19" ["20,BOOL"]="21"
        ["20,UOP"]="23" ["20,NL"]="11" ["21,cl"]="r BOOLA BOOL" ["21,or"]="r BOOLA BOOL"
        ["21,and"]="r BOOLA BOOL" ["21,rp"]="r BOOLA BOOL" ["22,cl"]="r BOOL STMT"
        ["22,or"]="r BOOL STMT" ["22,and"]="r BOOL STMT" ["22,rp"]="r BOOL STMT"
        ["22,ne"]="s 36" ["22,eq"]="s 37" ["22,gt"]="s 38" ["22,lt"]="s 39"
        ["22,ge"]="s 40" ["22,le"]="s 41" ["22,strgt"]="s 42" ["22,strlt"]="s 43"
        ["22,streq"]="s 44" ["22,strne"]="s 45" ["22,BOP"]="35" ["23,ld"]="s 9"
        ["23,ex"]="s 24" ["23,str"]="s 10" ["23,nl"]="s 12" ["23,STMT"]="22"
        ["23,IF"]="3" ["23,FORIN"]="4" ["23,INCLUDE"]="5" ["23,BUILTIN"]="6"
        ["23,VAR"]="7" ["23,STR"]="8" ["23,BOOL"]="46" ["23,UOP"]="23" ["23,NL"]="11"
        ["24,ld"]="r UOP ex" ["24,ex"]="r UOP ex" ["24,str"]="r UOP ex"
        ["24,nl"]="r UOP ex" ["25,in"]="s 47" ["26,str"]="s 10" ["26,nl"]="s 12"
        ["26,STR"]="48" ["26,NL"]="11" ["27,ld"]="r ARGS" ["27,rd"]="r ARGS"
        ["27,str"]="r ARGS" ["27,nl"]="r ARGS" ["27,ARGS"]="49"
        ["28,ld"]="r VAR ld ID rd" ["28,cl"]="r VAR ld ID rd" ["28,rd"]="r VAR ld ID rd"
        ["28,elif"]="r VAR ld ID rd" ["28,else"]="r VAR ld ID rd"
        ["28,or"]="r VAR ld ID rd" ["28,and"]="r VAR ld ID rd"
        ["28,rp"]="r VAR ld ID rd" ["28,ne"]="r VAR ld ID rd" ["28,eq"]="r VAR ld ID rd"
        ["28,gt"]="r VAR ld ID rd" ["28,lt"]="r VAR ld ID rd" ["28,ge"]="r VAR ld ID rd"
        ["28,le"]="r VAR ld ID rd" ["28,strgt"]="r VAR ld ID rd"
        ["28,strlt"]="r VAR ld ID rd" ["28,streq"]="r VAR ld ID rd"
        ["28,strne"]="r VAR ld ID rd" ["28,str"]="r VAR ld ID rd"
        ["28,nl"]="r VAR ld ID rd" ["28,$"]="r VAR ld ID rd" ["29,ld"]="s 52"
        ["29,str"]="s 10" ["29,nl"]="s 12" ["29,VAR"]="50" ["29,STR"]="51"
        ["29,NL"]="11" ["30,ld"]="s 52" ["30,str"]="s 10" ["30,nl"]="s 12"
        ["30,VAR"]="53" ["30,STR"]="54" ["30,NL"]="11" ["31,ld"]="r DOC"
        ["31,rd"]="r DOC" ["31,elif"]="r DOC" ["31,else"]="r DOC" ["31,str"]="r DOC"
        ["31,nl"]="r DOC" ["31,DOC"]="55" ["32,ld"]="s 9" ["32,lp"]="s 20"
        ["32,ex"]="s 24" ["32,str"]="s 10" ["32,nl"]="s 12" ["32,STMT"]="22"
        ["32,IF"]="3" ["32,FORIN"]="4" ["32,INCLUDE"]="5" ["32,BUILTIN"]="6"
        ["32,VAR"]="7" ["32,STR"]="8" ["32,BOOLA"]="56" ["32,BOOL"]="21" ["32,UOP"]="23"
        ["32,NL"]="11" ["33,ld"]="s 9" ["33,lp"]="s 58" ["33,ex"]="s 24"
        ["33,str"]="s 10" ["33,nl"]="s 12" ["33,STMT"]="22" ["33,IF"]="3"
        ["33,FORIN"]="4" ["33,INCLUDE"]="5" ["33,BUILTIN"]="6" ["33,VAR"]="7"
        ["33,STR"]="8" ["33,BOOL"]="57" ["33,UOP"]="23" ["33,NL"]="11" ["34,or"]="s 32"
        ["34,rp"]="s 59" ["35,ld"]="s 9" ["35,str"]="s 10" ["35,nl"]="s 12"
        ["35,STMT"]="60" ["35,IF"]="3" ["35,FORIN"]="4" ["35,INCLUDE"]="5"
        ["35,BUILTIN"]="6" ["35,VAR"]="7" ["35,STR"]="8" ["35,NL"]="11"
        ["36,ld"]="r BOP ne" ["36,str"]="r BOP ne" ["36,nl"]="r BOP ne"
        ["37,ld"]="r BOP eq" ["37,str"]="r BOP eq" ["37,nl"]="r BOP eq"
        ["38,ld"]="r BOP gt" ["38,str"]="r BOP gt" ["38,nl"]="r BOP gt"
        ["39,ld"]="r BOP lt" ["39,str"]="r BOP lt" ["39,nl"]="r BOP lt"
        ["40,ld"]="r BOP ge" ["40,str"]="r BOP ge" ["40,nl"]="r BOP ge"
        ["41,ld"]="r BOP le" ["41,str"]="r BOP le" ["41,nl"]="r BOP le"
        ["42,ld"]="r BOP strgt" ["42,str"]="r BOP strgt" ["42,nl"]="r BOP strgt"
        ["43,ld"]="r BOP strlt" ["43,str"]="r BOP strlt" ["43,nl"]="r BOP strlt"
        ["44,ld"]="r BOP streq" ["44,str"]="r BOP streq" ["44,nl"]="r BOP streq"
        ["45,ld"]="r BOP strne" ["45,str"]="r BOP strne" ["45,nl"]="r BOP strne"
        ["46,cl"]="r BOOL UOP BOOL" ["46,or"]="r BOOL UOP BOOL"
        ["46,and"]="r BOOL UOP BOOL" ["46,rp"]="r BOOL UOP BOOL" ["47,ld"]="r ARGS"
        ["47,cl"]="r ARGS" ["47,str"]="r ARGS" ["47,nl"]="r ARGS" ["47,ARGS"]="61"
        ["48,rd"]="s 62" ["49,ld"]="s 9" ["49,rd"]="s 63" ["49,str"]="s 10"
        ["49,nl"]="s 12" ["49,STMT"]="64" ["49,IF"]="3" ["49,FORIN"]="4"
        ["49,INCLUDE"]="5" ["49,BUILTIN"]="6" ["49,VAR"]="7" ["49,STR"]="8"
        ["49,NL"]="11" ["50,rd"]="s 65" ["51,rd"]="s 66" ["52,id"]="s 17" ["52,ID"]="67"
        ["53,rd"]="s 68" ["54,rd"]="s 69" ["55,ld"]="s 9" ["55,rd"]="r ELIF"
        ["55,elif"]="r ELIF" ["55,else"]="r ELIF" ["55,str"]="s 10" ["55,nl"]="s 12"
        ["55,STMT"]="2" ["55,IF"]="3" ["55,FORIN"]="4" ["55,INCLUDE"]="5"
        ["55,BUILTIN"]="6" ["55,VAR"]="7" ["55,STR"]="8" ["55,ELIF"]="70" ["55,NL"]="11"
        ["56,cl"]="r BOOLO BOOLO or BOOLA" ["56,or"]="r BOOLO BOOLO or BOOLA"
        ["56,and"]="s 33" ["56,rp"]="r BOOLO BOOLO or BOOLA"
        ["57,cl"]="r BOOLA BOOLA and BOOL" ["57,or"]="r BOOLA BOOLA and BOOL"
        ["57,and"]="r BOOLA BOOLA and BOOL" ["57,rp"]="r BOOLA BOOLA and BOOL"
        ["58,ld"]="s 9" ["58,lp"]="s 20" ["58,ex"]="s 24" ["58,str"]="s 10"
        ["58,nl"]="s 12" ["58,STMT"]="22" ["58,IF"]="3" ["58,FORIN"]="4"
        ["58,INCLUDE"]="5" ["58,BUILTIN"]="6" ["58,VAR"]="7" ["58,STR"]="8"
        ["58,BOOLO"]="71" ["58,BOOLA"]="19" ["58,BOOL"]="21" ["58,UOP"]="23"
        ["58,NL"]="11" ["59,cl"]="r BOOLA lp BOOLO rp" ["59,or"]="r BOOLA lp BOOLO rp"
        ["59,and"]="r BOOLA lp BOOLO rp" ["59,rp"]="r BOOLA lp BOOLO rp"
        ["60,cl"]="r BOOL STMT BOP STMT" ["60,or"]="r BOOL STMT BOP STMT"
        ["60,and"]="r BOOL STMT BOP STMT" ["60,rp"]="r BOOL STMT BOP STMT"
        ["61,ld"]="s 9" ["61,cl"]="s 72" ["61,str"]="s 10" ["61,nl"]="s 12"
        ["61,STMT"]="64" ["61,IF"]="3" ["61,FORIN"]="4" ["61,INCLUDE"]="5"
        ["61,BUILTIN"]="6" ["61,VAR"]="7" ["61,STR"]="8" ["61,NL"]="11"
        ["62,ld"]="r INCLUDE ld include cl STR rd"
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
        ["62,nl"]="r INCLUDE ld include cl STR rd"
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
        ["63,str"]="r BUILTIN ld ID cl ARGS rd" ["63,nl"]="r BUILTIN ld ID cl ARGS rd"
        ["63,$"]="r BUILTIN ld ID cl ARGS rd" ["64,ld"]="r ARGS ARGS STMT"
        ["64,cl"]="r ARGS ARGS STMT" ["64,rd"]="r ARGS ARGS STMT"
        ["64,str"]="r ARGS ARGS STMT" ["64,nl"]="r ARGS ARGS STMT"
        ["65,ld"]="r VAR ld ID or VAR rd" ["65,cl"]="r VAR ld ID or VAR rd"
        ["65,rd"]="r VAR ld ID or VAR rd" ["65,elif"]="r VAR ld ID or VAR rd"
        ["65,else"]="r VAR ld ID or VAR rd" ["65,or"]="r VAR ld ID or VAR rd"
        ["65,and"]="r VAR ld ID or VAR rd" ["65,rp"]="r VAR ld ID or VAR rd"
        ["65,ne"]="r VAR ld ID or VAR rd" ["65,eq"]="r VAR ld ID or VAR rd"
        ["65,gt"]="r VAR ld ID or VAR rd" ["65,lt"]="r VAR ld ID or VAR rd"
        ["65,ge"]="r VAR ld ID or VAR rd" ["65,le"]="r VAR ld ID or VAR rd"
        ["65,strgt"]="r VAR ld ID or VAR rd" ["65,strlt"]="r VAR ld ID or VAR rd"
        ["65,streq"]="r VAR ld ID or VAR rd" ["65,strne"]="r VAR ld ID or VAR rd"
        ["65,str"]="r VAR ld ID or VAR rd" ["65,nl"]="r VAR ld ID or VAR rd"
        ["65,$"]="r VAR ld ID or VAR rd" ["66,ld"]="r VAR ld ID or STR rd"
        ["66,cl"]="r VAR ld ID or STR rd" ["66,rd"]="r VAR ld ID or STR rd"
        ["66,elif"]="r VAR ld ID or STR rd" ["66,else"]="r VAR ld ID or STR rd"
        ["66,or"]="r VAR ld ID or STR rd" ["66,and"]="r VAR ld ID or STR rd"
        ["66,rp"]="r VAR ld ID or STR rd" ["66,ne"]="r VAR ld ID or STR rd"
        ["66,eq"]="r VAR ld ID or STR rd" ["66,gt"]="r VAR ld ID or STR rd"
        ["66,lt"]="r VAR ld ID or STR rd" ["66,ge"]="r VAR ld ID or STR rd"
        ["66,le"]="r VAR ld ID or STR rd" ["66,strgt"]="r VAR ld ID or STR rd"
        ["66,strlt"]="r VAR ld ID or STR rd" ["66,streq"]="r VAR ld ID or STR rd"
        ["66,strne"]="r VAR ld ID or STR rd" ["66,str"]="r VAR ld ID or STR rd"
        ["66,nl"]="r VAR ld ID or STR rd" ["66,$"]="r VAR ld ID or STR rd"
        ["67,rd"]="s 28" ["67,or"]="s 29" ["67,and"]="s 30"
        ["68,ld"]="r VAR ld ID and VAR rd" ["68,cl"]="r VAR ld ID and VAR rd"
        ["68,rd"]="r VAR ld ID and VAR rd" ["68,elif"]="r VAR ld ID and VAR rd"
        ["68,else"]="r VAR ld ID and VAR rd" ["68,or"]="r VAR ld ID and VAR rd"
        ["68,and"]="r VAR ld ID and VAR rd" ["68,rp"]="r VAR ld ID and VAR rd"
        ["68,ne"]="r VAR ld ID and VAR rd" ["68,eq"]="r VAR ld ID and VAR rd"
        ["68,gt"]="r VAR ld ID and VAR rd" ["68,lt"]="r VAR ld ID and VAR rd"
        ["68,ge"]="r VAR ld ID and VAR rd" ["68,le"]="r VAR ld ID and VAR rd"
        ["68,strgt"]="r VAR ld ID and VAR rd" ["68,strlt"]="r VAR ld ID and VAR rd"
        ["68,streq"]="r VAR ld ID and VAR rd" ["68,strne"]="r VAR ld ID and VAR rd"
        ["68,str"]="r VAR ld ID and VAR rd" ["68,nl"]="r VAR ld ID and VAR rd"
        ["68,$"]="r VAR ld ID and VAR rd" ["69,ld"]="r VAR ld ID and STR rd"
        ["69,cl"]="r VAR ld ID and STR rd" ["69,rd"]="r VAR ld ID and STR rd"
        ["69,elif"]="r VAR ld ID and STR rd" ["69,else"]="r VAR ld ID and STR rd"
        ["69,or"]="r VAR ld ID and STR rd" ["69,and"]="r VAR ld ID and STR rd"
        ["69,rp"]="r VAR ld ID and STR rd" ["69,ne"]="r VAR ld ID and STR rd"
        ["69,eq"]="r VAR ld ID and STR rd" ["69,gt"]="r VAR ld ID and STR rd"
        ["69,lt"]="r VAR ld ID and STR rd" ["69,ge"]="r VAR ld ID and STR rd"
        ["69,le"]="r VAR ld ID and STR rd" ["69,strgt"]="r VAR ld ID and STR rd"
        ["69,strlt"]="r VAR ld ID and STR rd" ["69,streq"]="r VAR ld ID and STR rd"
        ["69,strne"]="r VAR ld ID and STR rd" ["69,str"]="r VAR ld ID and STR rd"
        ["69,nl"]="r VAR ld ID and STR rd" ["69,$"]="r VAR ld ID and STR rd"
        ["70,rd"]="r ELSE" ["70,elif"]="s 74" ["70,else"]="s 75" ["70,ELSE"]="73"
        ["71,or"]="s 32" ["71,rp"]="s 76" ["72,ld"]="r DOC" ["72,rd"]="r DOC"
        ["72,str"]="r DOC" ["72,nl"]="r DOC" ["72,DOC"]="77" ["73,rd"]="s 78"
        ["74,ld"]="s 9" ["74,lp"]="s 20" ["74,ex"]="s 24" ["74,str"]="s 10"
        ["74,nl"]="s 12" ["74,STMT"]="22" ["74,IF"]="3" ["74,FORIN"]="4"
        ["74,INCLUDE"]="5" ["74,BUILTIN"]="6" ["74,VAR"]="7" ["74,STR"]="8"
        ["74,BOOLO"]="79" ["74,BOOLA"]="19" ["74,BOOL"]="21" ["74,UOP"]="23"
        ["74,NL"]="11" ["75,cl"]="s 80" ["76,cl"]="r BOOLA BOOLA and lp BOOLO rp"
        ["76,or"]="r BOOLA BOOLA and lp BOOLO rp"
        ["76,and"]="r BOOLA BOOLA and lp BOOLO rp"
        ["76,rp"]="r BOOLA BOOLA and lp BOOLO rp" ["77,ld"]="s 9" ["77,rd"]="s 81"
        ["77,str"]="s 10" ["77,nl"]="s 12" ["77,STMT"]="2" ["77,IF"]="3"
        ["77,FORIN"]="4" ["77,INCLUDE"]="5" ["77,BUILTIN"]="6" ["77,VAR"]="7"
        ["77,STR"]="8" ["77,NL"]="11" ["78,ld"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,cl"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,rd"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,elif"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,else"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,or"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,and"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,rp"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,ne"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,eq"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,gt"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,lt"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,ge"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,le"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,strgt"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,strlt"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,streq"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,strne"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,str"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,nl"]="r IF ld if BOOLO cl DOC ELIF ELSE rd"
        ["78,$"]="r IF ld if BOOLO cl DOC ELIF ELSE rd" ["79,cl"]="s 82"
        ["79,or"]="s 32" ["80,ld"]="r DOC" ["80,rd"]="r DOC" ["80,str"]="r DOC"
        ["80,nl"]="r DOC" ["80,DOC"]="83"
        ["81,ld"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,cl"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,rd"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,elif"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,else"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,or"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,and"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,rp"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,ne"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,eq"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,gt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,lt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,ge"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,le"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,strgt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,strlt"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,streq"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,strne"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,str"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,nl"]="r FORIN ld for ID in ARGS cl DOC rd"
        ["81,$"]="r FORIN ld for ID in ARGS cl DOC rd" ["82,ld"]="r DOC"
        ["82,rd"]="r DOC" ["82,elif"]="r DOC" ["82,else"]="r DOC" ["82,str"]="r DOC"
        ["82,nl"]="r DOC" ["82,DOC"]="84" ["83,ld"]="s 9" ["83,rd"]="r ELSE else cl DOC"
        ["83,str"]="s 10" ["83,nl"]="s 12" ["83,STMT"]="2" ["83,IF"]="3"
        ["83,FORIN"]="4" ["83,INCLUDE"]="5" ["83,BUILTIN"]="6" ["83,VAR"]="7"
        ["83,STR"]="8" ["83,NL"]="11" ["84,ld"]="s 9"
        ["84,rd"]="r ELIF ELIF elif BOOLO cl DOC"
        ["84,elif"]="r ELIF ELIF elif BOOLO cl DOC"
        ["84,else"]="r ELIF ELIF elif BOOLO cl DOC" ["84,str"]="s 10" ["84,nl"]="s 12"
        ["84,STMT"]="2" ["84,IF"]="3" ["84,FORIN"]="4" ["84,INCLUDE"]="5"
        ["84,BUILTIN"]="6" ["84,VAR"]="7" ["84,STR"]="8" ["84,NL"]="11"
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
