# TAC²ER

<b align="center">🗲 Lighning fast [AC²E](https://github.com/matthschw/ace) backend generator 🗲</b>

Just fill out a config like the ones in the `examples/` directory and run

```bash
$ tacer someop.yaml
```

This will create a folder with the name of the OPid containing both
`properties.json` and `input.scs` ready for use with
[AC²E](https://github.com/matthschw/ace).

## Installation and Usage

```bash
$ git clone https://github.com/augustunderground/tacer.git

$ pushd tacer

$ stack build

$ stack install

$ tacer examples/op2.yaml
```
