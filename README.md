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
$ tree ./op2
op2
├── ff_233.scs
├── ff_273.scs
├── ff_300.raw
├── ff_300.scs
├── ff_323.scs
├── ff_373.scs
├── ff_423.scs
├── fs_233.scs
├── fs_273.scs
├── fs_300.scs
├── fs_323.scs
├── fs_373.scs
├── fs_423.scs
├── mcg_233.scs
├── mcg_273.scs
├── mcg_300.scs
├── mcg_323.scs
├── mcg_373.scs
├── mcg_423.scs
├── op2.scs
├── properties.json
├── sf_233.scs
├── sf_273.scs
├── sf_300.scs
├── sf_323.scs
├── sf_373.scs
├── sf_423.scs
├── ss_233.scs
├── ss_273.scs
├── ss_300.scs
├── ss_323.scs
├── ss_373.scs
└── ss_423.scs

0 directories, 33 files
```

**NOTE:** You need the right PDK to simulate these netlists with spectre.
