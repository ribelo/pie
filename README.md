# Camelpie

OCaml/OxCaml voice assistant.

## Install

Build the Nix package:

```bash
nix build --impure .#pie
```

<!-- etabuild-hh3e -->

Build and install the `pie` binary into the shared OxCaml/opam switch:

```bash
nix develop .#oxcaml -c sh -lc 'dune build --profile=release @install && dune install camelpie'
```

Verify the installed command:

```bash
readlink -f "$(which pie)"
pie --help
```

The expected binary target is:

```text
/home/ribelo/.cache/opam/5.4.0+ox/bin/pie
```
