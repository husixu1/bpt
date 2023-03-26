#!/bin/bash

# Setup & Teardown ============================================================
setup_suite() {
    source ../bpt.sh

    # Generate some test files for testing includes
    tmp_dir="$(mktemp -d)"

    # Regular dependency
    echo "{{var0}}{{include: '${tmp_dir}/1.tpl'}}{{var0}}" >|"${tmp_dir}/0.tpl"
    echo "{{var1}}{{include: '${tmp_dir}/2.tpl'}}{{var1}}" >|"${tmp_dir}/1.tpl"
    echo "{{var2}}{{var2}}" >|"${tmp_dir}/2.tpl"

    # Circular dependency
    echo "{{var3}}{{include: '${tmp_dir}/4.tpl'}}{{var3}}" >|"${tmp_dir}/3.tpl"
    echo "{{var4}}{{include: '${tmp_dir}/5.tpl'}}{{var4}}" >|"${tmp_dir}/4.tpl"
    echo "{{var5}}{{include: '${tmp_dir}/3.tpl'}}{{var5}}" >|"${tmp_dir}/5.tpl"

    # Missing include
    echo "{{var6}}{{include: '${tmp_dir}/7.tpl'}}{{var6}}" >|"${tmp_dir}/6.tpl"
    echo "{{include: 'asdfghjkl'}}" >|"${tmp_dir}/7.tpl"
}

teardown_suite() {
    rm -rf "${tmp_dir:?}"
}

# A helper function
gen() { bpt.main ge <<<"$1"; }

# Scanner Feature Tests =======================================================
# Legal Inputs ----------------------------------------------------------------
test_newlines() {
    assert_equals 'abc'$'\n''def' "$(gen '{{var or "abc'$'\n''def"}}')"
    assert_equals 'abc'$'\n\n''def' "$(gen '{{var or "abc'$'\n\n''def"}}')"
    assert_equals $'\n''abc'$'\n''def'$'\n.' "$(gen $'\n''abc'$'\n''def'$'\n' && echo .)"

    # Newlines that is not str should be ignored
    assert_equals 'abc'$'\n''def' "$(gen '{{
        var
        or
        "abc'$'\n''def"
    }}')"
}

test_quotes_basic() {
    local var=''
    assert_equals 'abc' "$(gen "{{var or 'abc'}}")"
    assert_equals 'abc' "$(gen '{{var or "abc"}}')"
}

test_quotes_mixed() {
    assert_equals "abc'def'" "$(gen "{{var or \"abc'def'\"}}")"
    assert_equals 'abc"def"' "$(gen "{{var or 'abc\"def\"'}}")"
    assert_equals '"abc' "$(gen "{{var or '\"abc'}}")"
}

test_quotes_escaped() {
    assert_equals 'abc"def' "$(gen '{{var or "abc\"def"}}')"
    assert_equals "abc'def" "$(gen "{{var or 'abc\\'def'}}")"
    assert_equals "abc''def" "$(gen "{{var or 'abc\\'\\'def'}}")"
    assert_equals "abc'1\"def" "$(gen "{{var or 'abc\\'1\"def'}}")"
}

test_delimiters() {
    local var=100
    assert_equals 100 "$(bpt.main ge -l '{' -r '}' <<<'{var}')"
    assert_equals 100 "$(bpt.main ge -l '[[' -r ']]' <<<'[[var]]')"
    assert_equals 100 "$(bpt.main ge -l '((' -r '))' <<<'((var))')"
    assert_equals 100 "$(bpt.main ge -l '<<' -r '>>' <<<'<<var>>')"
    assert_equals 100 "$(bpt.main ge -l '^(' -r ')^' <<<'^(var)^')"
    assert_equals 100 "$(bpt.main ge -l 'abcd' -r 'efgh' <<<'abcdvarefgh')"
    assert_equals 100 "$(bpt.main ge -l '{==(' -r ')==}' <<<'{==(var)==}')"
    assert_equals 100 "$(bpt.main ge -l '^.[()|*$+?\{' -r '{\?+$*|)([.^' <<<'^.[()|*$+?\{var{\?+$*|)([.^')"
}

# Illegal Inputs --------------------------------------------------------------
test_illegal_quotes() {
    # unclosed quotes
    assert_fail 'gen "{{var or \"}}"'
    assert_fail 'gen "{{var or "'\''}}"'
    assert_fail "gen '{{var or \"\"\"}}'"
}

test_illegal_delimiters() {
    local var=100
    # ld and rd cannot contain spaces
    assert_fail 'bpt.main ge -l " " -r "}" <<<"{var}"'
    assert_fail 'bpt.main ge -l "{ {" -r "} }" <<<"{ {var} }"'

    # ld and rd cannot be the same
    assert_fail 'bpt.main ge -l "ab" -r "ab" <<<"abvarab"'
    assert_fail 'bpt.main ge -l "|" -r "|" <<<"|var|"'
}

# Parser Feature Tests ========================================================
# Legal Inputs ----------------------------------------------------------------
test_str() {
    assert_equals $'abc\n.' "$(gen $'abc\n' && echo .)"
    assert_equals $'abc\nabc\n\n.' "$(gen $'abc\nabc\n\n' && echo .)"
    assert_equals 'abc def \ ghi "jkl"' "$(gen 'abc def \ ghi "jkl"')"

    # test single-line with no returns
    assert_equals 'abc' "$(echo -n 'abc' | bpt.main ge)"
}

test_var() {
    local var0='' var1=100 var2=abc
    assert_equals '' "$(gen '{{var0}}')"
    assert_equals 100 "$(gen '{{var1}}')"
}

test_default_var() {
    local var0='' var1=100 var2=abc
    assert_equals 100 "$(gen '{{var0 or {{var1}}}}')"
    assert_equals 100 "$(gen '{{var1 or {{var2}}}}')"

    assert_equals '' "$(gen '{{var0 and {{var1}}}}')"
    assert_equals abc "$(gen '{{var1 and {{var2}}}}')"

    assert_equals 123 "$(gen '{{var0 or "123"}}')"
    assert_equals 100 "$(gen '{{var1 or "123"}}')"

    assert_equals '' "$(gen '{{var0 and "123"}}')"
    assert_equals 123 "$(gen '{{var1 and "123"}}')"

    assert_equals abc "$(gen '{{var1 and {{var0 or {{var2}}}}}}')"
    assert_equals 100 "$(gen '{{var0 or {{var1 or "abc"}}}}')"
}

test_bool_basics() {
    assert_equals 1 "$(gen '{{if "" : "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if "a" : "0" else: "1"}}')"
}

test_bool_args() {
    local var1='' var2='a'
    assert_equals 1 "$(gen '{{if {{var1}}: "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if {{var2}}: "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if "x"{{var1}}: "0" else: "1"}}')"

    assert_equals 0 "$(gen '{{if {{if {{var1}}: "" else: {{var2}}}} : "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if {{if {{var2}}: "" else: {{var1}}}} : "0" else: "1"}}')"

    assert_equals 0 "$(gen '{{if {{for i in {{seq: "5"}}: {{i}}}} == "12345": "0" else: "1"}}')"
}

test_bool_compare() {
    assert_equals 0 "$(gen '{{if "0" -lt "1": "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if "0" -gt "1": "0" else: "1"}}')"

    assert_equals 0 "$(gen '{{if "0" -le "1": "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if "0" -le "0": "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if "0" -ge "1": "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if "0" -ge "0": "0" else: "1"}}')"

    assert_equals 0 "$(gen '{{if "0" -eq "0": "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if "0" -ne "0": "0" else: "1"}}')"

    assert_equals 1 "$(gen '{{if "abc" > "def": "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if "abc" < "abf": "0" else: "1"}}')"

    assert_equals 1 "$(gen '{{if "abc" == "def": "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if "abc" != "abf": "0" else: "1"}}')"
}

test_bool_nested() {
    local var1='' var2='a'
    assert_equals 1 "$(gen '{{if "0" -lt "1" and "1" -lt "0": "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if ("0" -lt "1") and "1" -lt "0": "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if "0" -lt "1" and (""): "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if (("0" -lt "1") and "1" -lt "0"): "0" else: "1"}}')"

    assert_equals 1 "$(gen '{{if "1" and "1" and "0" and "0": "1"}}')"
    assert_equals 1 "$(gen '{{if "1" and "1" or "0" and "0": "1"}}')"

    assert_equals 0 "$(gen '{{if "0" -lt "1" or ("1" -lt "0"): "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if ("0" -lt "1") or ("1" -lt "0"): "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if ((("0" -lt "1"))) or ("a" != "b"): "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if ("a" < "b") and (("0" -gt "1") or ("1" -lt "0")): "0" else: "1"}}')"

    assert_equals 1 "$(gen '{{if !"": "1"}}')"
    assert_equals 0 "$(gen '{{if !"a": "1" else: "0"}}')"
    assert_equals 0 "$(gen '{{if "a" and ! "a": "1" else: "0"}}')"
    assert_equals 1 "$(gen '{{if "a" or ! "a": "1" else: "0"}}')"

    assert_equals 1 "$(gen '{{if "a" or ! ("" and "a"): "1"}}')"
    assert_equals 0 "$(gen '{{if "a" and ! ("a" or ""): "1" else: "0"}}')"
    assert_equals 0 "$(gen '{{if ("a" and "b") and ! ("a" or ""): "1" else: "0"}}')"

    assert_equals 1 "$(gen '{{if !(("0" -lt "1") or ("1" -lt "0")): "0" else: "1"}}')"
    assert_equals 0 "$(gen '{{if !!!!("0" -lt "1") or ("1" -lt "0"): "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if !(!(!("0" -lt "1"))): "0" else: "1"}}')"
}

test_if_basic() {
    local var=1
    assert_equals '' "$(gen '{{if "": "0"}}')"
    assert_equals 0 "$(gen '{{if "1": "0"}}')"

    assert_equals 10 "$(gen '{{if "1": {{var}}"0"}}')"
    assert_equals 01 "$(gen '{{if "1": "0"{{var}}}}')"
    assert_equals 011 "$(gen '{{if "1": "0"{{var}}"1"}}')"

    assert_equals 123450 "$(gen '{{if "1": {{for i in {{seq: "5"}}: {{i}}}}"0"}}')"
}

test_if_elif_else() {
    assert_equals 1 "$(gen '{{if "": "0" else: "1"}}')"
    assert_equals 1 "$(gen '{{if "": "0" elif "1": "1"}}')"
    assert_equals 1 "$(gen '{{if "": "0" elif "a": "1" else: "2"}}')"
    assert_equals 2 "$(gen '{{if "": "0" elif "": "1" else: "2"}}')"
    assert_equals 3 "$(gen '{{if "": "0" elif "": "1" elif "1": "3" else: "2"}}')"
    assert_equals '' "$(gen '{{if "": "0" elif "": "1"}}')"
}

test_if_nested() {
    assert_equals 00 "$(gen '{{if "1": {{if "1": "0" else: "1"}}"0" else: "1"}}')"
    assert_equals 01 "$(gen '{{if "": "0" else: {{if "1": "0" else: "1"}}"1"}}')"
    assert_equals 01 "$(gen '{{if "": "0" elif "1": {{if "1": "0" elif "": "1"}}"1"}}')"
}

test_forin() {
    assert_equals '1.2.3.4.5.' "$(gen '{{for i in "1" "2" "3" "4" "5": {{i}}"."}}')"
    assert_equals '1 2 3 4 5.' "$(gen '{{for i in "1 2 3 4 5": {{i}}"."}}')"
    assert_equals '1.2.3.4.5.' "$(gen '{{for i in {{seq: "5"}}: {{i}}"."}}')"
    assert_equals '12.12.12.' "$(gen '{{for i in {{seq: "3"}}: {{for i in {{seq: "2"}}: {{i}}}}"."}}')"
    assert_equals '112.212.312.' "$(gen '{{for i in {{seq: "3"}}: {{i}}{{for j in {{seq: "2"}}: {{j}}}}"."}}')"
}

test_builtin() {
    assert_equals $'1 2 3' "$(gen '{{seq: "3"}}')"
    assert_equals '3' "$(gen '{{len: "abc"}}')"
    assert_equals '1.2.3.' "$(gen '{{for i in "1" "2" "3": {{i}}"."}}')"
    assert_equals '123.' "$(gen '{{for i in {{quote: "1" "2" "3"}}: {{i}}"."}}')"
    assert_equals '1 2 3.' "$(gen '{{for i in "1 2 3": {{i}}"."}}')"
    assert_equals '1.2.3.' "$(gen '{{for i in {{split: "1 2 3"}}: {{i}}"."}}')"
    assert_equals '{{' "$(gen '{{quote: "{{"}}')"
}

test_include() {
    local var0=0 var1=1 var2=2 var3=3 var4=4 var5=5 var6=6
    assert_equals '22' "$(gen "{{include: \"${tmp_dir}/2.tpl\"}}")"
    assert_equals '1221' "$(gen "{{include: \"${tmp_dir}/1.tpl\"}}")"
    assert_equals '012210' "$(gen "{{include: \"${tmp_dir}/0.tpl\"}}")"
}

# Test mixed statements
test_document() {
    local var0 var1 var2 doc
    read -r -d '' doc <<-EOF
123 {{var0}}
{{for i in {{seq: "3"}}:
    {{if {{i}} -ge {{var2}}:
        {{i}}"."{{var1}}"."{{ quote: {{seq: "3"}} }}" "
    }}
}}456abc
EOF
    var0=0 var1=1 var2=2
    assert_equals '123 0'$'\n''2.1.123 3.1.123 456abc' "$(gen "$doc")"
    var0=abc var1=3 var2=3
    assert_equals '123 abc'$'\n''3.3.123 456abc' "$(gen "$doc")"
}

test_generate_eval() {
    local var0 var1 var2 doc script="$tmp_dir/generated.sh"
    read -r -d '' doc <<-EOF
123 {{var0}}
{{for i in {{seq: "3"}}:
    {{if {{i}} -ge {{var2}}:
        {{i}}"."{{var1}}"."{{ quote: {{seq: "3"}} }}" "
    }}
}}456abc
EOF
    bpt.main g <<<"$doc" >|"$script"

    # Separate generate-eval and one-step ones should produce the same result
    var0=0 var1=1 var2=2
    export var0 var1 var2
    assert_equals "$(gen "$doc")" "$(bash "$script")"
}

# Illegal Inputs --------------------------------------------------------------
test_illegal_var() {
    local var0='' var1=100
    assert_fail 'gen "{{var0"'
    assert_fail 'gen "{{var0 or }}"'
    assert_fail 'gen "{{var0 and }}"'
    assert_fail "gen '"'{{var0 and "asdf" "adsf"}}'"'"
}

test_illegal_bool() {
    assert_fail "gen '"'{{if " : "0"}}'"'"
    assert_fail "gen '"'{{if a" : "0"}}'"'"

    assert_fail "gen '"'{{if "0" -aa "1" : "0"}}'"'"
    assert_fail "gen '"'{{if -eq : "0"}}'"'"
    assert_fail "gen '"'{{if "0" -gt : "0"}}'"'"
    assert_fail "gen '"'{{if -lt "0": "0"}}'"'"

    assert_fail "gen '"'{{if "1" -gt !"2"}}'"'"
    assert_fail "gen '"'{{if "a" > ! "b"}}'"'"

    assert_fail "gen '"'{{if ("0": "0"}}'"'"
    assert_fail "gen '"'{{if "0"): "0"}}'"'"
    assert_fail "gen '"'{{if (("0"): "0"}}'"'"
}

test_illegal_if() {
    assert_fail "gen '"'{{if}}'"'"
    assert_fail "gen '"'{{if "a" }}'"'"
    assert_fail "gen '"'{{if "a": }}'"'"
    assert_fail "gen '"'{{if "a": "a" else }}'"'"
    assert_fail "gen '"'{{if "a": "a" else: }}'"'"
    assert_fail "gen '"'{{if "a": else: "a"}}'"'"
    assert_fail "gen '"'{{if "a": "a" elif: "a" }}'"'"
    assert_fail "gen '"'{{if "a": "a" elif "a": "a" else}}'"'"
    assert_fail "gen '"'{{if "a": "a" elif "a": "a" else: }}'"'"
    assert_fail "gen '"'{{if "a": "a" elif : "a" else: "a"}}'"'"
}

test_illegal_forin() {
    assert_fail "gen '"'{{for i }}'"'"
    assert_fail "gen '"'{{for i in "1": }}'"'"
    assert_fail "gen '"'{{for i in {{i}}"."}}'"'"
    assert_fail "gen '"'{{for i in : {{i}}"."}}'"'"
    assert_fail "gen '"'{{for i "1" "2" "3" "4" "5"  {{i}}"."}}'"'"
    assert_fail "gen '"'{{for i "1" "2" "3" "4" "5": {{i}}"."}}'"'"
}

test_illegal_builtin() {
    assert_fail "gen '"'{{asdf: }}'"'"
    assert_fail "gen '"'{{asdf: "asdf"}}'"'"
}

test_illegal_include() {
    local err
    err="$(gen "{{include: \"${tmp_dir}/3.tpl\"}}" 2>&1 >/dev/null)"
    assert_matches '.*cyclic include.*' "$err"
    err="$(gen "{{include: \"${tmp_dir}/4.tpl\"}}" 2>&1 >/dev/null)"
    assert_matches '.*cyclic include.*' "$err"
    err="$(gen "{{include: \"${tmp_dir}/5.tpl\"}}" 2>&1 >/dev/null)"
    assert_matches '.*cyclic include.*' "$err"

    local err
    err="$(gen "{{include: \"${tmp_dir}/6.tpl\"}}" 2>&1 >/dev/null)"
    assert_matches '.*not exist.*' "$err"
    err="$(gen "{{include: \"${tmp_dir}/7.tpl\"}}" 2>&1 >/dev/null)"
    assert_matches '.*not exist.*' "$err"
}
