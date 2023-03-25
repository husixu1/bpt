# About

BPT - **B**ash **P**ure **T**emplate (Engine)

**Features:**

- **Pure:** The rendering process is pure, ensuring that the rendered result is fully predictable and consistent across multiple runs.
    - This is the core feature and also the purpose of this template engine. This feature makes `bpt` suitable for generating a large number of small configuration files without unexpected changes due to an environment variable change or an external file change when re-generating config files.
    - A fingerprinting function is provided to digest all the inputs, including files, variables, and the engine itself.
    - Functionalities are also provided to collect all the variables and includes used in a template.
- **Minimal dependency:** `bpt` requires only minimal external programs in addition to bash itself. All the external programs required are within coreutils, which is most likely bundled with the Linux installation.
    - `bash >= 5`
    - `md5sum`, `sort`, `uniq`, and `cat`.
- **Single bash script:** The `bpt.sh` script contains everything you need.
    - It can be called directly or used as a library by sourcing it in another bash script.
    - It works out of the box.
- **A little bit of extra functionalities:** While a simple template engine that only replaces `{{var}}`s can also be easily made pure, the functionalities is somewhat limited. BPT provides a little bit extra that makes life easier.
    - Delimiters can be replaced.
    - Branching, looping, and basic boolean operations are supported.
    - Recursive inclusion of templates is also supported.
    - Refer to [Writing Templates](#writing-templates) for more details.

**Deficiencies:**

- **Slow:** Although optimization has been made, a LALR(1) parser implemented in bash is still quite slow. It is not recommended to use `bpt` for large and complex templates such as an HTML templating engine.
    - The startup overhead is around 16ms.
    - Rendering a template consisting of 100 variables takes around 75ms.
    - Rendering a template consisting of 1000 variables takes around 900ms, with most of the time spent on reduction.
    - Refer to [#Benchmarking](#benchmarking) for more details.

# Installation & Usages

All you need is `bpt.sh`. Place it anywhere you'd like, then `chmod +x bpt.sh`.

**Direct call:**

```bash
var1=a var2=b ./bpt.sh ge template.tpl     # Render the template
var1=a var2=b ./bpt.sh f template.tpl      # Fingerprint the template
./bpt.sh g template.tpl > out.sh           # Generate reusable script
./bpt.sh g -l '<<' -r '>>' ge template.tpl # Customize delimiters
./bpt.sh cv template.tpl                   # Collect variables
./bpt.sh ci template.tpl                   # Collect includes
./bpt.sh -h                                # Print help
```
**As a library:**

```bash
#!/bin/bash
source bpt.sh
export var1=a var2=b

mapfile -t vars <<<"$(bpt.main cv template.tpl)"
mapfile -t includes <<<"$(bpt.main ci template.tpl)"
script="$(bpt.main g template.tpl)"
render_result="$(bpt.main ge template.tpl)"
```

# Writing Templates

BPT employs a straightforward grammar whereby any content beyond the top-level delimiters is treated as strings, whereas anything enclosed by delimiters is considered either a string or a keyword. Notably, any string located within these delimiters must be enclosed in single or double quotes.

## Examples

### Basic Variable Replacement

```
{{var}}
{{var or "abc"}}      # Use "abc" if var is empty
{{var or {{var2}}}}   # Use {{var2}} if var is empty
{{var and "abc"}}     # Use "abc" if var is not empty
{{var and {{var2}}}}  # Use {{var2}} if var is not empty
```

### Include Another Template

All include paths are relative to where the script is called from (`$PWD`).

```
{{include: "another.tpl"}}
```

### Branching

```
# Test emptiness of strings
{{if "a": "yes"}}
{{if {{var}}: "yes" else: "no"}}
{{if {{var1}}: "1" elif {{var2}}: "2" else: "3"}}
{{if {{var1}}: "1" elif {{var2}}: "2" elif {{var3}}: "3" else: "4"}}

# Compare integers
{{if "1" -lt "2": "yes"}}
{{if "1" -gt "2": "no"}}
{{if "1" -le "2": "yes"}}
{{if "1" -ge "2": "no"}}
{{if "1" -eq "2": "no"}}
{{if "1" -ne "2": "yes"}}

# Compare strings
{{if "abc" < "def": "yes"}}
{{if "abc" > "def": "no"}}
{{if "abc" == "def": "yes"}}
{{if "abc" != "def": "no"}}
{{if ! "abc" == "def": "yes"}}

# Combined comparison
{{if "a" or "": "yes"}}
{{if ("1" -lt "2" or "") and (("a" < "b") or "abc"): "yes"}}
```

### Looping

```
{{for var in "a" "b" "c": {{var}}","}}
{{for var in {{seq: "5"}}: {{var}}","}}
```

### Builtin Functions

Currently we only support `seq`, `len` and `quote`. The `seq` builtin is a wrapper of the `seq` executable in coreutils. The other two are implemented in pure bash.

```
{{len: "abc"}}              # 3
{{seq: "3"}}                # 1 2 3
{{seq: "1" "3" "10"}}'      # 1 4 7 10
{{quote: {{seq: "3"}}}}     # 123
```

### Mixture of the above

```bash
obj1=Pen obj2=Apple obj3=Pineapple ./bpt.sh ge <<-EOF
{{for i in {{seq: "2"}}:
  {{for j in "Uh! " "":
    {{if "i" -eq "1":
      {{for obj in {{obj2}} {{obj3}}:
        {{if {{j}}:
           "I have a " {{obj1}} ", I have "
           {{if {{obj}} == "Apple": "a "}} {{obj}}",
"       }}
        {{j}} {{obj}}"-"{{obj1}}",
"     }}
      else:
        {{j}} {{obj1}}"-"{{obj3}}"-"{{obj2}}"-"{{obj1}}".
"   }}
  }}
}}

EOF

I have a Pen, I have a Apple,
Uh! Apple-Pen,
I have a Pen, I have Pineapple,
Uh! Pineapple-Pen,
Apple-Pen,
Pineapple-Pen,
Uh! Pen-Pineapple-Apple-Pen.
Pen-Pineapple-Apple-Pen.
```

## The BPT Grammar

For reference, the complete BNF of bpt is as follows:

```
DOC     -> DOC STMT
         | .
STMT    -> IF | FORIN | INCLUDE | BUILTIN | VAR | STR .
IF      -> ld if BOOLS cl DOC ELIF ELSE rd .
ELIF    -> ELIF elif BOOLS cl DOC
         | .
ELSE    -> else cl DOC
         | .
BOOLS   -> BOOLO
         | UOP BOOLS .
BOOLO   -> BOOLO or BOOLA
         | BOOLO or UOP BOOLA
         | BOOLA .
BOOLA   -> BOOLA and BOOL
         | BOOLA and UOP BOOL
         | BOOLA and lp BOOLS rp
         | BOOLA and UOP lp BOOLS rp
         | lp BOOLS rp
         | BOOL .
BOOL    -> ARGS BOP ARGS
         | ARGS .
FORIN   -> ld for ID in ARGS cl DOC rd .
INCLUDE -> ld include cl STR rd .
BUILTIN -> ld ID cl ARGS rd .
ARGS    -> ARGS STMT
         | STMT .
VAR     -> ld ID rd
         | ld ID or VAR rd
         | ld ID or STR rd
         | ld ID and VAR rd
         | ld ID and STR rd .
BOP     -> ne | eq | gt | lt | ge | le | strgt | strlt | streq | strne .
UOP     -> ex .
ID      -> id .
STR     -> str .
```

And token mappings:

```
ld    -> left delimiter, default {{
rd    -> right delimiter, default }}
cl    -> :      ex    -> !      lp    -> (      rp    -> )
eq    -> -eq    ne    -> -ne    gt    -> -gt    lt    -> -lt    ge -> -ge   le -> -le
streq -> ==     strne -> ! ->   strgt -> >      strlt -> <
id    -> [[:alpha:]_][[:alnum:]_]*
str   -> Everything outside the toplevel ld...rd or inside "..." or '...' in ld...rd
```

# Benchmarking

Comparisons were made among some pure-bash template engines that work out of the box in two different settings:

- Rendering a simple plain text "aaa": This measures the startup overhead of the engine.
- Rendering 1000 variables `${var}`/`{{var}}`/`<%var%>` whose content is set to `a`: This measures the variable render performance.

We have set the plain bash processing/variable replacement as the baseline. Each test is run 12 times, with the first 2 times as warmups. You can find more details in the benchmark.sh file. If you want to run the benchmarks yourself, you'll need to install [hyperfine][hyperfine].

The plain bash processing/variable replacement was set as the baseline. Each test was run 12 times, with the first 2 times as warmups. More details can be found in the [benchmark.sh](benchmark/benchmark.sh) file. If you want to run the benchmarks yourself, [hyperfine][hyperfine] needs to be installed.

The following results were obtained with Intel(R) Core(TM) i9-9900K CPU @ 3.60GHz:

**Startup Overhead**

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `bash -c 'echo "aaa"'` | 2.6 ± 0.5 | 1.6 | 3.1 | 1.00 |
| `./shtpl 0vars.tpl` | 4.0 ± 0.0 | 3.9 | 4.0 | 1.53 ± 0.30 |
| `./renderest 0vars.tpl` | 5.2 ± 0.1 | 5.1 | 5.4 | 2.02 ± 0.40 |
| `. <(./bash-tpl 0vars.tpl)` | 116.6 ± 48.3 | 50.9 | 177.2 | 45.02 ± 20.66 |
| `./mo 0vars.tpl` | 24.0 ± 15.0 | 10.2 | 50.9 | 9.27 ± 6.07 |
| `./bpt.sh ge 0vars.tpl` | 47.2 ± 17.5 | 14.0 | 57.0 | 18.23 ± 7.63 |

**Render 1000 Identical Variables**

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `var=a bash 1000vars.sh` | 1.7 ± 1.6 | 0.9 | 4.7 | 1.00 |
| `var=a ./shtpl < 1000vars.shtpl` | 6.3 ± 0.0 | 6.2 | 6.3 | 3.69 ± 3.41 |
| `var=a ./renderest 1000vars.tpl` | 9.9 ± 0.1 | 9.8 | 10.0 | 5.85 ± 5.40 |
| `var=a . <(./bash-tpl 1000vars.bash-tpl)` | 883.4 ± 16.9 | 861.5 | 906.6 | 520.47 ± 480.42 |
| `var=a ./mo 1000vars.tpl` | 3200.0 ± 92.6 | 3106.5 | 3405.4 | 1885.37 ± 1740.78 |
| `var=a ./bpt.sh ge 1000vars.tpl` | 895.1 ± 14.8 | 886.6 | 935.6 | 527.36 ± 486.76 |

Engines that do complete parsing ([mo][mo], [bash-tpl][bash-tpl] and [bpt][bpt]) are slower by orders of magitude than engines that only do pattern matching and replacement (plain bash, [shtpl][shtpl] and [renderest][renderest]). A simple profiling of bpt shows that most of its time are spent on parsing (compared to lexing and evaluating the output).

# Contributing

Issues & pull requests of any kind are welcome.

Adding functionalities shall not conflict with or break existing features.

# Credits

- [Grammophone][grammophone] is used to generate the LALR parse table.
- [bash_unit][bash_unit] is used for testing.
- [hyperfine][hyperfine] is used for benchmarking.

# License

MIT. See the [LICENSE](LICENSE) file.

<!-- References --->
[bash_unit]:https://github.com/pgrange/bash_unit
[grammophone]:https://github.com/mdaines/grammophone
[hyperfine]:https://github.com/sharkdp/hyperfine

[shtpl]:https://github.com/mlorenzo-stratio/shtpl
[renderest]:https://github.com/relaxdiego/renderest
[bash-tpl]:https://github.com/TekWizely/bash-tpl
[mo]:https://github.com/tests-always-included/mo
[bpt]:https://github.com/husixu1/bpt
