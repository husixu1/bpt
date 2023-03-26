# About

BPT - **B**ash **P**ure **T**emplate (Engine)

<p align="center"><img src="res/bpt.svg"></img></div>

**Features:**

- **Pure:** The rendering process resembles a [pure function][pure_function], ensuring that the rendered results are fully predictable and consistent across multiple runs given the same inputs. It also produces no side-effects other than the generated script or the render result.
    - This feature makes `bpt` suitable for generating a large number of small configuration files without unexpected changes due to an environment variable change or an external file change when re-generating config files.
    - A fingerprinting function is provided to digest all the inputs, including template files, variables, and the engine itself.
    - Functionalities are also provided to collect all the variables and includes used in a template.
- **Minimal dependency:** `bpt` requires only a minimal set of external programs in addition to bash. All the external programs required are in coreutils, which is most likely bundled with your Linux installation.
    - `bash >= 5`
    - coreutils: `md5sum`, `sort`, `uniq`, `cat`, `mktemp`, ...
- **Single bash script:** The `bpt.sh` script contains everything you need.
    - It can be called directly or used as a library by sourcing it in another bash script.
    - It works out of the box.
- **A little bit of extra functionalities:** While a simple template engine that only replaces `{{var}}`s can also be easily made pure, their functionalities are somewhat limited. BPT provides a little bit extra that makes life easier.
    - Delimiters can be replaced.
    - Branching, list iteration, and basic boolean operations are supported.
    - Recursive inclusion of templates is also supported.
    - Refer to [Writing Templates](#writing-templates) for more details.

**Deficiencies:**

- **Slow:** Although optimizations have been made, a LALR(1) parser implemented in bash is still quite slow. It is not recommended to use `bpt` for large and complex templates.
    - The startup overhead is around 16ms.
    - Rendering a template consisting of 100 variables takes around 75ms.
    - Rendering a template consisting of 1000 variables takes around 900ms, with most of the time spent on reduction.
    - Refer to [#Benchmarks](#benchmarks) for more details.

# Installation & Usages

All you need is `bpt.sh`. Place it anywhere you'd like, then `chmod +x bpt.sh`.

**Direct call:**

```bash
var1=a var2=b ./bpt.sh ge template.tpl     # Render the template
var1=a var2=b ./bpt.sh f template.tpl      # Fingerprint the template
./bpt.sh g template.tpl > out.sh           # Generate reusable script
./bpt.sh g -l '<<' -r '>>' template.tpl    # Customize delimiters
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
fingerprint="$(bpt.main f template.tpl)"
```

# Writing Templates

BPT employs a straightforward grammar whereby any content beyond the top-level delimiters is treated as strings, whereas anything enclosed by delimiters is considered either a string, an identifier, or a keyword. Notably, any string located within these delimiters must be enclosed in single or double quotes.

## Examples

### Variable Replacement

```
{{var}}
{{var or "abc"}}      # Use "abc" if var is empty
{{var or {{var2}}}}   # Use {{var2}} if var is empty
{{var and "abc"}}     # Use "abc" if var is not empty
{{var and {{var2}}}}  # Use {{var2}} if var is not empty
```

Bash internal variables can also be used:

```
{{RANDOM}}, {{BASH_VERSION}}, {{HOSTNAME}}, {{PWD}}, ...
```

**Note:** It's obvious that using non-deterministic variables such as `{{RANDOM}}` or `{{SRANDOM}}` results in impure fingerprinting/rendering processes. `$RANDOM` can produce varying results in two subsequent evaluations. It is thus recommended to use `rand=$RANDOM ./bpt ge ...` and use `{{rand}}` in templates to let the `fingerprint` command correctly capture the non-determinism of `$RANDOM`.

### Include Another Template

All include paths are relative to where the script is called from (`$PWD`). Includes are processed recursively.

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
{{if "abc" =~ "a.*": "yes"}}

# Combined comparison
{{if "a" or "": "yes"}}
{{if ("1" -lt "2" or "") and (("a" < "b") or "abc"): "yes"}}

# Shorthands
{{""}}                    # 'false'
{{"a"}}                   # 'true'
{{"a": "yes"}}            # 'yes'
{{{{var}}: "yes": "no"}}  # 'yes' if {{var}} is not empty, otherwise 'no'
```

### Iterate a List

```
{{for var in "a" "b" "c": {{var}}","}}
{{for var in {{seq: "5"}}: {{var}}","}}
```

### Builtin Functions

Currently we only support `seq`, `len`, `quote`, `cat` and `split`. The `seq` builtin is a wrapper of the `seq` executable in coreutils. The others are implemented in pure bash.

```
{{len: "abc"}}                                  # 3
{{seq: "3"}}                                    # 1 2 3
{{seq: "1" "3" "10"}}                           # 1 4 7 10
```

```
{{quote: "2 3" "3" }}                           # 2 33
{{for i in {{quote: "2 3" "3" }}: {{i}}","}}    # 2 33,

{{cat: "2 3" "3" }}                             # 2 33
{{for i in {{cat: "2 3" "3" }}: {{i}}","}}      # 2,33,

{{split: "2 3" "3" }}                           # 2 3 3
{{for i in {{split: "2 3" "3" }}: {{i}}","}}    # 2,3,3,
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
```
```
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
STMT    -> IF | FORIN | INCLUDE | BUILTIN | QUOTE | VAR | STR .
IF      -> ld if BOOLS cl DOC ELIF ELSE rd
         | ld BOOLS cl DOC cl DOC rd
         | ld BOOLS cl DOC rd
         | ld BOOLS rd .
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
QUOTE   -> ld quote cl ARGS rd .
ARGS    -> ARGS STMT
         | STMT .
VAR     -> ld ID rd
         | ld ID or VAR rd
         | ld ID or STR rd
         | ld ID and VAR rd
         | ld ID and STR rd .
BOP     -> ne     | eq    | gt    | lt    | ge    | le
         | strne  | streq | strgt | strlt | strcm .
UOP     -> ex .
ID      -> id .
STR     -> str .
```

Token mappings:

```
ld    -> left delimiter, default {{
rd    -> right delimiter, default }}
cl    -> :      ex    -> !      lp    -> (      rp    -> )
eq    -> -eq    ne    -> -ne    gt    -> -gt    lt    -> -lt    ge -> -ge   le -> -le
streq -> ==     strne -> ! ->   strgt -> >      strlt -> <
id    -> [[:alpha:]_][[:alnum:]_]*
str   -> Everything outside the toplevel ld...rd or inside "..." or '...' in ld...rd
```

# Benchmarks

Comparisons were made among some pure-bash template engines that work out of the box in two different settings:

- Rendering a simple plain text "aaa": This measures the startup overhead of the engine.
- Rendering 1000 variables `${var}`/`{{var}}`/`<%var%>` whose content is set to `a`: This measures the variable render performance.

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

Engines that do complete parsing ([mo][mo], [bash-tpl][bash-tpl] and [bpt][bpt]) are slower by orders of magitude than engines that only do pattern matching and replacement (plain bash, [shtpl][shtpl] and [renderest][renderest]). A simple profiling of bpt shows that most of its time are spent on the parsing part (time spent on lexing and evaluating the output is relatively negligible).

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
[pure_function]:https://en.wikipedia.org/wiki/Pure_function
[bash_unit]:https://github.com/pgrange/bash_unit
[grammophone]:https://github.com/mdaines/grammophone
[hyperfine]:https://github.com/sharkdp/hyperfine

[shtpl]:https://github.com/mlorenzo-stratio/shtpl
[renderest]:https://github.com/relaxdiego/renderest
[bash-tpl]:https://github.com/TekWizely/bash-tpl
[mo]:https://github.com/tests-always-included/mo
[bpt]:https://github.com/husixu1/bpt
