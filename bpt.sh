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
# $2: Scanner function (default to cat, i.e. read from stdin)
#   The scanner should output <token-type> \n <token-content> per token read.
# $3: Reduce function hook
#   Args that passed to this function:
#     $1: The reduce rule (as a string). i.e. "LHS RHS1 RHS2 ..."
#     ${@:1}: RHS contents.
#   The reduce function should store the reduce result to the `result` variable.
# $4: (optional) If set, enable debug.
shlr.parse() (
    local -rn table="$1"
    local -r scanner="${2:-cat}"
    local -r reduce_fn="${3:-echo}"
    if [[ -n $4 ]]; then local -r NDEBUG=false; else local -r NDEBUG=true; fi

    # Parse stack and associatied content stack
    local -a states=(0) contents=()

    # Look-ahead token and its content
    local token='' content=''

    # $1: Goto state after shift
    __shift() {
        $NDEBUG ||
            echo "[DBG] ${states[*]:${#states[@]}-1} Shift $1 \`$content\`" >&2
        states+=("$1")
        contents+=("$content")
        token='' content=''
    }

    # $1: Rule
    __reduce() {
        $NDEBUG ||
            echo "[DBG] ${states[*]:${#states[@]}-1} Reduce $1" >&2
        local -a rule=()
        read -ra rule <<<"$1"
        local num_rhs=$((${#rule[@]} - 1))
        local -a args=("${contents[@]:${#contents[@]}-$num_rhs}")

        # Reduce
        states=("${states[@]:0:${#states[@]}-$num_rhs}")
        contents=("${contents[@]:0:${#contents[@]}-$num_rhs}")

        # Goto state and save reduced content
        states+=("${table["${states[@]:${#states[@]}-1},${rule[0]}"]}")

        # Command substitution discards newlines. We need to preserve them.
        local result=''
        $reduce_fn "$1" "${args[@]}"
        contents+=("${result}")
    }

    # Simply print the result
    __accept() {
        $NDEBUG || echo "[DBG] Result accepted" >&2
        echo "${contents[0]}"
    }

    while true; do
        [[ -n $token ]] || {
            local OIFS="$IFS" && IFS=''
            read -r token
            read -r content
            IFS="$OIFS"
        }

        action="${table["${states[@]:${#states[@]}-1},$token"]}"
        case "$action" in
        s*) __shift "${action#s }" ;;  # Shift
        r*) __reduce "${action#r }" ;; # Reduce
        a) __accept && break ;;        # Accept
        '')
            # TODO: Parse error and locations
            # (location xxx, expecting xxx but got xxx)
            echo "Error in template: STATES ${states[*]} TOKEN ${token} CONTENT ${content}." >&2
            exit 1
            ;;
        *) # Parse Table Error
            echo "Internal error: STATES ${states[*]} TOKEN ${token} CONTENT ${content}. " >&2
            echo "Internal error: action '$action' not recognized." >&2
            exit 1
            ;;
        esac
    done < <("$scanner")
)

# The tokenizer for bpt
# $1: Left deilmiter
# $2: Right delimiter
#
# Token name to content mappings:
#   str: Anything outside the toplevel `ld ... rd` or
#        Anything inside `"..."` or `'...'` within any `ld ... rd`
#     Note1: `"` inside `"..."` needs to be escaped using `\"`,
#            and the same for `'` inside `'...'`.
#     Note2: str cannot contain newline since both the input and the output
#            of the scanner is line-based. Newline is expressed by the nl token.
#   nl: \n
#   ld: ${ldelim}
#   rd: ${rdelim}
#   cl: :
#   eq: ==
#   ne: !=
#   lt: <
#   gt: >
#   lt: <=
#   gt: >=
#   id: [[:alpha:]_][[:alnum:]_]*
bpt.scan() {
    local -r ld="$1" rd="$2"
    local -r e_ld="$(printf '%q' "$ld")" e_rd="$(printf '%q' "$rd")"
    bpt.__test_delims "$ld" "$rd" || return 1

    # Keywords
    local -r KW_RE="${e_ld}|${e_rd}|==|!=|>|<|>=|<=|:|\"|\'|and|or|if|elif|else|for|in|include"
    local -r ID_RE='[[:alpha:]_][[:alnum:]_]*'

    # Scanner states
    local num_ld=0
    local quote=''

    # Location trackers
    local num_lines=1
    local num_bytes=0

    # Tokenizer
    local line='' content=''
    while read -r line; do
        # Only count newlines outside `ld ... rd` and inside quotes.
        if [[ $num_lines -gt 1 && ($num_ld -eq 0 || -n $quote) ]]; then
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
                    echo ld
                elif [[ $line =~ (${e_ld}) ]]; then
                    content="${line%%"${BASH_REMATCH[1]}"*}"
                    echo str
                else
                    content="$line"
                    echo str
                fi
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
                    echo str
                    echo "$string"
                }
            elif [[ $line =~ ^(${KW_RE}) ]]; then
                # Inside `ld ... rd` and matches a keyword at front
                content="${BASH_REMATCH[1]}"
                case "$content" in
                '==') echo eq ;;
                '!=') echo ne ;;
                '<') echo lt ;;
                '>') echo gt ;;
                '<=') echo le ;;
                '>=') echo ge ;;
                ':') echo cl ;;
                '"' | "'") quote="$content" ;;
                "$ld")
                    ((++num_ld))
                    echo ld
                    ;;
                "$rd")
                    [[ -n $num_ld ]] || {
                        echo "Extra '$rd'."
                        return 1
                    }
                    ((--num_ld))
                    echo rd
                    ;;
                and | or | if | elif | else) ;&
                for | in | include) echo "$content" ;;
                *)
                    echo "Internal error: Unrecognized token ${content}" >&2
                    return 1
                    ;;
                esac
                [[ -n $quote ]] || echo "$content"
            else # Inside `ld ... rd` but outside quotes
                # Ignore spaces inside `ld ... rd`
                [[ $line =~ ^[[:space:]]+(.*) ]] && {
                    line="${BASH_REMATCH[1]}"
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
                echo id
                echo "$content"
            fi
            line="${line#"$content"}"
        done
        ((++num_lines))
    done
    echo $  # The EOF token
    echo '' # The EOF content (empty)
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
    # Note1: When using position argument ($@), index is 1-based.
    # Note2: Avoid copy ${@:2} to an array, which is slow.
    local -a rule=()
    read -ra rule <<<"$1"
    shift

    case "${rule[0]}" in
    VAR) result="$2" ;;
    STMT) [[ ${rule[1]} != VAR ]] || result="$1"$'\n' ;;
    *) printf -v result "%s" "${@}" ;;
    esac
}

bpt.__reduce_collect_includes() {
    local -a rule=()
    read -ra rule <<<"$1"
    shift

    case "${rule[0]}" in
    STR) result="$1" ;;
    INCLUDE) result="$3" ;;
    STMT) [[ ${rule[1]} != INCLUDE ]] || result="$1"$'\n' ;;
    *) printf -v result "%s" "$@" ;;
    esac
}

# The reduce function to collect all top-level strings, discarding all else.
bpt.__reduce_collect_toplevel_strings() {
    local -a rule=()
    read -ra rule <<<"$1"
    shift

    case "${rule[0]}" in
    STR) result="$1" ;;
    NL) result=$'\n' ;;
    STMT) [[ ${rule[1]} != STR ]] || result="$1" ;;
    *) printf -v result "%s" "$@" ;;
    esac
}

# The reduce function to generate the template
bpt.__reduce_generate() {
    local -a rule=()
    read -ra rule <<<"$1"
    shift

    case "${rule[0]}" in
    NL) result=$'\n' ;;
    STR) result="$1" ;;
    IDENTIFIER) result="$1" ;;
    VAR) result="\${$2}" ;;
    INCLUDE) result="$(__recursive_process <"$3")" ;;
    FORIN) result="for $3 in \$($5); do $7 done" ;;
    BOOL)
        # Strip the tag from STMT
        local stmt_l_type="${1%%:*}"
        local stmt_l="${1#*:}"
        local -a contents=()
        case $stmt_l_type in
        IDENTIFIER | STR) contents[0]="\$(echo -n $(printf %q "${stmt_l}"))" ;;
        VAR) contents[0]="\"$stmt_l\"" ;;
        *) contents[0]="\$(${stmt_l})" ;;
        esac
        # If rule is STMT op STMT, deal with op and RHS
        [[ ${#rule[@]} -eq 2 ]] || {
            contents[1]="-${rule[2]}"
            local stmt_r_type="${3%%:*}"
            local stmt_r="${3#*:}"
            case $stmt_r_type in
            IDENTIFIER | STR)
                contents[2]="\$(echo -n $(printf %q "${stmt_r}"))"
                ;;
            VAR) contents[2]="\"$stmt_r\"" ;;
            *) contents[2]="\$(${stmt_r})" ;;
            esac
        }
        printf -v result "%s" "[[ " "${contents[@]}" " ]]"
        ;;
    BOOLS)
        if [[ ${#rule[@]} -eq 2 ]]; then
            result="$1"
        else
            case "${rule[2]}" in
            and) result="$1 && $3" ;;
            or) result="$1 || $3" ;;
            esac
        fi
        ;;
    ELSE) [[ ${#rule[@]} -eq 1 ]] || result="else $3" ;;
    ELIF) [[ ${#rule[@]} -eq 1 ]] ||
        result="$1; elif $3; then $5" ;;
    IF) result="if $3; then ${*:5:3} fi" ;;
    STMT)
        # Tag the sub-type to the reduce result
        # (Need to strip the tag wherever STMT is used)
        result="${rule[1]}:$1"
        ;;
    DOCUMENT)
        # Return when document is empty
        [[ ${#rule[@]} -ne 1 ]] || return

        # Strip the tag from STMT
        local stmt_type="${2%%:*}"
        local stmt="${2#*:}"

        # Reduce the document
        result="$1"
        case "$stmt_type" in
        STR | IDENTIFIER) result+="{ echo -n $(printf %q "$stmt"); };" ;;
        VAR) result+="{ echo -n \"$stmt\"; };" ;;
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
# $4: (optional) If set, enable debug.
#
# Input: Template from stdin
#
# Grammar:
#   DOCUMENT   -> DOCUMENT STMT
#               | .
#   STMT       -> IF | FORIN | INCLUDE | VAR | IDENTIFIER | STR .
#   IF         -> ld if BOOLS cl DOCUMENT ELIF ELSE rd .
#   ELIF       -> ELIF elif BOOLS cl DOCUMENT
#               | .
#   ELSE       -> else cl DOCUMENT
#               | .
#   BOOLS      -> BOOLS and BOOL
#               | BOOLS or BOOL
#               | BOOL .
#   BOOL       -> STMT eq STMT
#               | STMT ne STMT
#               | STMT gt STMT
#               | STMT lt STMT
#               | STMT ge STMT
#               | STMT le STMT
#               | STMT .
#   FORIN      -> ld for IDENTIFIER in DOCUMENT cl DOCUMENT rd .
#   INCLUDE    -> ld include STR rd .
#   VAR        -> ld IDENTIFIER rd .
#   IDENTIFIER -> id .
#   STR        -> str | NL .
#   NL         -> nl .
#
# Note: the `IDENTIFIER -> id`, `STR -> str | NL`, and `NL -> nl` rules are not
#   redundant. They are for better controls when hooking the reduce function.
bpt.process() (
    local -r ld="$1" rd="$2" debug="$4"
    local -r reduce_fn="${3:-bpt.__reduce_generate}"

    # Curry this function so that it can be called by the reducer recursively
    __recursive_process() { bpt.process "$ld" "$rd" "$reduce_fn" "$debug"; }
    export -f __recursive_process

    # Curry the scanner so that it can be passed to the parser
    scanner() { bpt.scan "$ld" "$rd"; }

    # Prase with the provided reduce function
    shlr.parse PARSE_TABLE scanner "$reduce_fn" "$debug"
)

bpt.print_help() {
    :
}

bpt.main() {
    local ld='{{' rd='}}'
    local input_file=''
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
            post_process='echo'
            ;;
        -ci | --collect-includes)
            reduce_fn=bpt.__reduce_collect_includes
            post_process='echo'
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
            [[ -z $input_file ]] || {
                bpt.print_help
                return 1
            }
            input_file="$1"
            ;;
        esac
        shift
    done

    if $scan; then
        bpt.scan "$ld" "$rd" <"$input_file"
    else
        # Process templates
        result="$(bpt.process "$ld" "$rd" "$reduce_fn" "$debug" <"$input_file")"
        var=apple
        var1=10
        var2=20
        var3=durian
        var4=eggplant
        var100=100
        list1="1 2 3"
        $post_process "$result"
    fi
}

(return 0 2>/dev/null) || bpt.main "$@"
