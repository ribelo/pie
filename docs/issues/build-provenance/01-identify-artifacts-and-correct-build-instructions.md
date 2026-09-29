# Identify artifacts and correct build instructions

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Canonical source and fresh artifact provenance include selected native code archives and TLS stubs.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete (PIE028). Helper fixtures 5/5 plus owned `pie.exe` record at `.scratch/pie-record-run4/` (binary + `pie.provenance.json`) are the acceptance checkpoint. Later source changes require a **new** record; do not reuse that fingerprint. Registered all-test compile and screened integration remain pending. This is not whole-audit green.

## Requirements

- When PIE produces a deployment artifact, its build record shall identify the compiler, PIE source and resolved Ox inputs. ^piebuild-py7n
- PIE build instructions shall name existing shell targets and packages and distinguish mutable development dependencies from locked package inputs. ^piebuild-un48

## Evidence

The default Nix development shell imports the shared opam switch. It does not install the flake’s locked Ox source.

Round39 observed compiler5.4.0+ox, Dune3.22.2 and installed Ox META version d06548b. The flake locks Eta09dab48f9b320e17e34c5ccc7a42d10230f7f437. Current Ox checkout HEAD is a05377162350aaecc82f901dfacd79134bc72dc3, with separate accepted uncommitted work.

README18 names the absent .#oxcaml shell and camelpie package. The flake exports the default shell, and Dune declares package pie.

The install helper pins committed master for PIE and installs committed Ox through another helper. It does not deploy uncommitted audit decisions.

The running binary matches the installed opam binary. It differs from tracked bin/pie. Installed library META does not establish the dependencies embedded in that executable.

## Disposition

Correct README targets and installation semantics. State the actual supported and checked toolchain. Do not describe the mutable development shell as a hermetic package build.

Record source identity, dirty-source status and resolved dependency identity beside each deployment artifact. Use a small build record, not a runtime environment dump.

Remove tracked installation copies under bin/, lib/pie/ and doc/pie/ after caller and packaging reconciliation. Keep authoritative source under packages/pie/. Do not remove the active installed binary or user data.

Separate build, test execution, installation and service activation in the instructions. Installation or activation needs explicit authorization during this audit.

## Acceptance

Check documented commands against declared targets without installing during tests. Compile the application and registered tests. Run only the screened unit gate.

Identify the selected artifact before activation. Its build record must distinguish committed, modified and installed inputs.

## Ox prerequisites

Accepted Ox changes remain incoming until PIE builds and tests against their actual artifact. A lock entry, checkout HEAD or current META file alone is insufficient.
