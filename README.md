# Reregex, yet another minimal regex implementation

## Example: Grep

```bash
cat README.md | cargo run --example grep -- '`[^`]+`'
```

## Example: Visualize regex

```bash
cargo run --example dot -- '-?([1-9][0-9]*)?[13579]' > odd.dot
dot -Tpng test.dot > test.png
```

## Features

* Greedy and non-greedy matching, e.g. `.*?`
* Character set ranges `[a-z]`
* Alternations `(a|bb)+`
* Graphviz dot-formatted NFA diagrams

## Not supported

* Capture groups
* Lookarounds
* Unicode-awareness
* Fast execution