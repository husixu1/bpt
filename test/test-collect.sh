#!/bin/bash

# Setup & Teardown ============================================================
setup_suite() {
    source ../bpt.sh
    source ../bpt.sh
    gen() { bpt.main ge <<<"$1"; }

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

    # Regular dependency with duplicate vars
    echo "{{var0}}{{include: '${tmp_dir}/9.tpl'}}{{var0}}" >|"${tmp_dir}/8.tpl"
    echo "{{var0}}{{var1}}{{include: '${tmp_dir}/10.tpl'}}{{var1}}" >|"${tmp_dir}/9.tpl"
    echo "{{var2}}{{var0}}{{var2}}{{var1}}" >|"${tmp_dir}/10.tpl"

    for f in "i"{01..10}".tpl"; do
        echo "abc $f" >|"${tmp_dir}/$f"
    done
}

teardown_suite() {
    rm -rf "${tmp_dir:?}"
}

# Helper functions
cv() { bpt.main cv <<<"$1"; }
ci() { bpt.main ci <<<"$1"; }
cvf() { bpt.main cv "$1"; }
cif() { bpt.main ci "$1"; }

# Tests =======================================================================
test_collect_vars() {
    assert_equals '' "$(cv 'abcdef')"
    assert_equals 'var1'$'\n''var2' "$(cv 'abc{{var1}}def{{var2}}ghi')"

}

test_collect_vars_nested() {
    # Variables collected in nested statements
    assert_equals 'var'$'\n''var1' "$(cv '{{var or {{var1}}}}')"
    assert_equals 'var'$'\n''var1' "$(cv '{{var or {{var1 or "123"}}}}')"
    assert_equals 'var'$'\n''var1'$'\n''var2' "$(cv '{{var or {{var1 and {{var2}}}}}}')"
    assert_equals 'var1'$'\n''var2' "$(cv '{{seq: {{var1 and {{var2}}}} "123"}}}')"

    # Variables indside forin loop body should not be collected
    assert_equals '' "$(cv 'abc{{for var in "": "def"}}ghi')"
    assert_equals '' "$(cv 'abc{{for var in "": {{var}}"def"}}ghi')"
    assert_equals 'var' "$(cv '{{var}} abc {{for var in "": {{var}}"def"}} ghi')"
    assert_equals 'var1' "$(cv '{{var1}} abc {{for var in "": {{var}}"def"}} ghi')"
    assert_equals 'var'$'\n''var1' "$(cv '{{var1}} abc {{for var in "": {{var}}"def"}} {{var}} ghi')"
    assert_equals 'var1'$'\n''var2' "$(cv '{{var1}} abc {{for var in "": {{var}}{{var2}}"def"}} ghi')"
    assert_equals '' "$(cv 'abc {{for var in "": {{var}} {{for var1 in "1": {{var1}} }} }} def')"
    assert_equals 'var'$'\n''var1' "$(cv 'abc {{var}} {{for var in {{var1}}: {{var}}{{var1}} {{for var1 in "1": {{var1}} }} }} def')"

    assert_equals "$(printf '%s\n' v{01..10})" "$(cv '
        {{v10}}
        {{if ! {{v01}} > {{v02}}:
            {{v02}}
        elif {{v03}}:
            {{if ({{v04}} -gt {{v05}} or ({{v06}} < {{v07}})):
                {{v05}}"abc"{{v02}}
            elif {{v05}} -gt {{v02}} and ({{v03}} or {{v04}}):
                {{v02}}"abc"{{v02}}
            elif {{v06}}"asdf":
                {{for vvv in {{v07}} "aaa" {{seq: {{v08}}}}:
                    {{v09}}
                }} {{v10}}
            }}"1"
            {{len: {{v02}} {{v04}}}}
        else:
            {{v01}}
        }}')"
}

test_collect_vars_recursive() {
    # Variables collected in recursive included documents
    assert_equals 'var0'$'\n''var1'$'\n''var2' "$(cvf "$tmp_dir/0.tpl")"
    assert_equals 'var1'$'\n''var2' "$(cvf "$tmp_dir/1.tpl")"
    assert_equals 'var2' "$(cvf "$tmp_dir/2.tpl")"

    assert_fail "cvf $tmp_dir/3.tpl"
    assert_fail "cvf $tmp_dir/4.tpl"
    assert_fail "cvf $tmp_dir/5.tpl"

    assert_fail "cvf $tmp_dir/6.tpl"
    assert_fail "cvf $tmp_dir/7.tpl"

    assert_equals 'var0'$'\n''var1'$'\n''var2' "$(cvf "$tmp_dir/8.tpl")"
    assert_equals 'var0'$'\n''var1'$'\n''var2' "$(cvf "$tmp_dir/9.tpl")"
    assert_equals 'var0'$'\n''var1'$'\n''var2' "$(cvf "$tmp_dir/10.tpl")"
}

test_collect_includes() {
    assert_equals "$tmp_dir/i02.tpl" "$(ci "{{include: \"$tmp_dir/i02.tpl\"}}")"
    assert_equals "$(printf '%s\n' "$tmp_dir/i"{01..10}".tpl")" "$(ci "
        {{include: \"$tmp_dir/i10.tpl\"}}
        {{if ! {{include: \"$tmp_dir/i01.tpl\"}} > {{include: \"$tmp_dir/i02.tpl\"}}:
            {{include: \"$tmp_dir/i02.tpl\"}}
        elif {{include: \"$tmp_dir/i03.tpl\"}}:
            {{if ({{include: \"$tmp_dir/i04.tpl\"}} -gt {{include: \"$tmp_dir/i05.tpl\"}}
                    or ({{include: \"$tmp_dir/i06.tpl\"}} < {{include: \"$tmp_dir/i07.tpl\"}})):
                {{include: \"$tmp_dir/i05.tpl\"}}'abc'{{include: \"$tmp_dir/i02.tpl\"}}
            elif {{include: \"$tmp_dir/i05.tpl\"}} -gt {{include: \"$tmp_dir/i02.tpl\"}}
                    and ({{include: \"$tmp_dir/i03.tpl\"}} or {{include: \"$tmp_dir/i04.tpl\"}}):
                {{include: \"$tmp_dir/i02.tpl\"}}'abc'{{include: \"$tmp_dir/i02.tpl\"}}
            elif {{include: \"$tmp_dir/i06.tpl\"}}'asdf':
                {{for vvv in {{include: \"$tmp_dir/i07.tpl\"}} 'aaa' {{seq: {{include: \"$tmp_dir/i08.tpl\"}}}}:
                    {{include: \"$tmp_dir/i09.tpl\"}}
                }} {{include: \"$tmp_dir/i10.tpl\"}}
            }}\"1\"
            {{len: {{include: \"$tmp_dir/i02.tpl\"}} {{include: \"$tmp_dir/i04.tpl\"}}}}
        else:
            {{include: \"$tmp_dir/i01.tpl\"}}
        }}")"
}

test_collect_includes_recursive() {
    assert_equals "$tmp_dir/1.tpl"$'\n'"$tmp_dir/2.tpl" "$(cif "$tmp_dir/0.tpl")"
    assert_equals "$tmp_dir/2.tpl" "$(cif "$tmp_dir/1.tpl")"

    assert_fail "cif $tmp_dir/3.tpl"
    assert_fail "cif $tmp_dir/4.tpl"
    assert_fail "cif $tmp_dir/5.tpl"

    assert_fail "cif $tmp_dir/6.tpl"
    assert_fail "cif $tmp_dir/7.tpl"
}
