#!/bin/bash

THISDIR="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")" && pwd)"

DEPS=(hyperfine curl)
for dep in "${DEPS[@]}"; do
    command -v "$dep" >/dev/null 2>&1 || {
        echo "'$dep' required but not found" >&2
        exit 1
    }
done

pushd "${THISDIR}" >/dev/null || exit 1
{
    # Download other template engines -----------------------------------------
    [[ -e mo ]] || {
        echo "Downloading mo ..."
        curl -SL https://git.io/get-mo -o mo &&
            chmod +x mo
    } || exit 1

    [[ -e bash-tpl ]] || {
        echo "Downloading bash-tpl ..."
        curl -SL https://github.com/TekWizely/bash-tpl/releases/download/v0.7.0/bash-tpl -o bash-tpl &&
            chmod +x bash-tpl
    } || exit 1

    [[ -e renderest ]] || {
        echo "Downloading renderest ..."
        curl -SL https://raw.githubusercontent.com/relaxdiego/renderest/master/render -o renderest &&
            chmod +x renderest
    }

    [[ -e shtpl ]] || {
        echo "Downloading shtpl ..."
        curl -SL https://raw.githubusercontent.com/mlorenzo-stratio/shtpl/main/shtpl -o shtpl &&
            chmod +x shtpl
    }

    ln -sf ../bpt.sh bpt.sh

    # Prepare test files ------------------------------------------------------
    echo "aaa" >|"0vars.tpl"
    { echo -n 'echo ' && for _ in {1..1000}; do echo -n "\${var}"; done; } >|"1000vars.sh"
    for _ in {1..1000}; do echo -n "\$var"; done >|"1000vars.shtpl"
    for _ in {1..1000}; do echo -n "{{var}}"; done >|"1000vars.tpl"
    for _ in {1..1000}; do echo -n "<%\$var%>"; done >|"1000vars.bash-tpl"

    # Benchmark ---------------------------------------------------------------
    hyperfine \
        --runs 10 --warmup 2 \
        --export-markdown result-0vars.md \
        "bash -c 'echo \"aaa\"'" \
        "./shtpl 0vars.tpl" \
        "./renderest 0vars.tpl" \
        ". <(./bash-tpl 0vars.tpl)" \
        "./mo 0vars.tpl" \
        "./bpt.sh ge 0vars.tpl"

    hyperfine \
        --runs 10 --warmup 2 \
        --export-markdown result-1000vars.md \
        "var=a bash 1000vars.sh" \
        "var=a ./shtpl < 1000vars.shtpl" \
        "var=a ./renderest 1000vars.tpl" \
        "var=a . <(./bash-tpl 1000vars.bash-tpl)" \
        "var=a ./mo 1000vars.tpl" \
        "var=a ./bpt.sh ge 1000vars.tpl"
}
popd >/dev/null || exit 1
