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

    # Same contents as 0, 1, 2
    echo "{{var0}}{{include: '${tmp_dir}/1.tpl'}}{{var0}}" >|"${tmp_dir}/8.tpl"
}

teardown_suite() {
    rm -rf "${tmp_dir:?}"
}

# Helper function
fp() { bpt.main f "$1"; }
fpi() { bpt.main f <<<"$1"; }

# Tests =======================================================================
test_fingerprint() {
    local var0 var1 var2 fp1 fp2

    # Fingerprint a regular template
    assert_fail 'fpi "{{abc}}"'
    assert 'abc= fpi "{{abc}}"'
    assert 'abc=1 fpi "{{abc}}"'

    # different vars
    var0=0 var1=0 var2=0
    fp1=$(fp "${tmp_dir}/0.tpl")
    var0=0 var1=1 var2=2
    fp2=$(fp "${tmp_dir}/0.tpl")
    assert_not_equals "$fp1" "$fp2"

    # Same vars but different contents
    fp1=$(fp "${tmp_dir}/0.tpl")
    fp2=$(fp "${tmp_dir}/1.tpl")
    assert_not_equals "$fp1" "$fp2"

    # Different filenames but same contents
    fp1=$(fp "${tmp_dir}/0.tpl")
    fp2=$(fp "${tmp_dir}/8.tpl")
    assert_equals "$fp1" "$fp2"
}

test_fingerprint_invalid() {
    # Vars not defiend
    assert_fail "fp ${tmp_dir}/0.tpl"
    assert_fail "fp ${tmp_dir}/1.tpl"
    assert_fail "fp ${tmp_dir}/2.tpl"

    # Cyclic includes
    assert_fail "fp ${tmp_dir}/3.tpl"
    assert_fail "fp ${tmp_dir}/4.tpl"
    assert_fail "fp ${tmp_dir}/5.tpl"
    assert_fail "fp ${tmp_dir}/7.tpl"

    # Cyclic includes with vars defined
    local var3=1 var4=1 var5=1 var6=1
    assert_fail "fp ${tmp_dir}/3.tpl"
    assert_fail "fp ${tmp_dir}/4.tpl"
    assert_fail "fp ${tmp_dir}/5.tpl"
    assert_fail "fp ${tmp_dir}/6.tpl"
    assert_fail "fp ${tmp_dir}/7.tpl"
}
