# Camelpie

OCaml/OxCaml voice assistant.

## Install

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
/home/ribelo/.cache/opam/5.2.0+ox/bin/pie
```
