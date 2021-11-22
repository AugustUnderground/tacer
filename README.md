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
```

This installs the `tacer` executable.

Generate the testbench `input.scs` and `properties.json` for a specific OP by
passing a corresponding `op#.yaml` to `tacer`.

```bash
$ tacer examples/op2.yaml
```

For all examples do:

```bash
$ for id (1,2,3,4,5,6,8,9); do tacer ./examples/op$id.yaml; done
```

These commands will create folders `op#` in the current working directory like
so:

```
$ tree ./op1
op1
├── input.scs
└── properties.json

0 directories, 2 files
```
