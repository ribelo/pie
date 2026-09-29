---
kind: requirement
---
# Ox dogfood builds

## Intent

Keep Pie continuously compatible with the evolving Ox codebase by making the committed tip of the local Ox `master` branch the source for normal builds. This capability covers Ox source selection for Pie builds; it does not publish Ox releases or include uncommitted Ox work.

## Requirements

- When a developer invokes `nix build --impure .#pie`, the Pie build shall resolve Ox from the commit at the local Ox checkout's `master` HEAD during that build. ^etabuild-pam0
- When the Pie Nix build resolves Ox, the Pie build shall exclude staged, unstaged, and untracked Ox changes. ^etabuild-zmr3
- The Pie Nix flake shall omit Ox from its `flake.lock` graph. ^etabuild-80a6
- If the Pie build cannot resolve or build the commit at the local Ox checkout's `master` HEAD, then the Pie build shall terminate with a build failure for that checkout. ^etabuild-z560
- When `pie-opam-install` installs Ox for Pie development, the installer shall resolve Ox from the commit at the local Ox checkout's `master` HEAD. ^etabuild-e0gr
- The Pie build documentation shall identify `nix build --impure .#pie` as the normal Nix package build command. ^etabuild-hh3e

## Verification seams

- Exercise the Nix package boundary with two successive local Ox `master` commits and observe that each build resolves the corresponding committed tree while ignoring dirty files.
- Inspect the flake lock graph to verify that it contains no Ox node.
- Exercise an incompatible Ox fixture and observe a build failure rather than a fallback resolution.
- Exercise `pie-opam-install` against a known local Ox `master` commit and inspect the resulting OPAM pin.
