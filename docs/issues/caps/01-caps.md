---
status: completed
package: pie
consumer: Pie assistant service and CLI
---

# Pie onto capabilities

## Problem Statement

Pie uses standard-library and Unix process, environment, console, clock and filesystem operations in library and command code without declaring the authority to do so. Ox's capability gate reports roughly 87 such calls in 20 production files. Pie also owns an IPC listener and optional OTLP exporter that require explicit capability grants.

## Solution

The Ox_app Door supplies the process powerbox to Pie's commands and service. Pie narrows that powerbox for each effectful function and uses Ox_caps wrappers instead of raw operations. Tests receive a powerbox through Ox_test, and Pie's fourth gate runs the installed Ox gate without a raw-call allow-list. No change to operational behaviour or production binary is part of this migration.

## User Stories

1. As a Pie maintainer, I want the assistant and commands to obtain capabilities from Ox_app.Env.caps and pass only the capability rows needed by each library function, so that signatures reveal authority without library code minting it. ^pcaps-6ppc
2. As a Pie maintainer, I want production file, environment, clock, console and process operations to use Ox_caps wrappers with the required capability in the caller's signature, so that no raw dangerous call remains under the gate. ^pcaps-62x0
3. As a Pie maintainer, I want the assistant's Ox_ipc.Listener.run call to receive filesystem-write and network capabilities, so that IPC socket ownership is explicit. ^pcaps-pdn5
4. As a Pie maintainer, I want the optional Ox_otel exporter to receive a random capability from the Door, so that trace ID creation declares its authority. ^pcaps-wury
5. As a Pie maintainer, I want tests of functions with new capability parameters to receive Ox_test.powerbox (), so that existing assertions exercise the same behavior with explicit authority. ^pcaps-n1rg
6. As a Pie maintainer, I want scripts/caps-gate to execute Ox's installed gate over Pie with no allow-list, so that production raw calls fail a fourth gate alongside build, tests and formatting. ^pcaps-uus2
7. As a Pie user, I want command output, assistant configuration, recording recovery and control-socket behavior preserved across the capability migration, so that granting authority does not change what Pie does. ^pcaps-x8ij

## Testing Decisions

Use Pie's current build, test and formatting gates in an isolated _build_r50p, then run scripts/caps-gate. Tests remain excluded from the production gate and mint their own test powerboxes; live services and installed Pie binary are not touched.

## Delivered

- The Door supplies caps to the CLI and assistant; production signatures declare the narrowest rows for environment, file access, time, standard output/error, IPC network, and OTel random. The tests supply `Ox_test.powerbox ()`.
- Against the private Ox install at `/tmp/ox-r50`, `dune build @install`, `dune test` (EIO_BACKEND=posix, -j 2), `dune build @fmt`, and Pie's installed `scripts/caps-gate` all passed. The caps gate has no allow-list.
- A separate intermittent fake-helper child-reaping hang in `test_prepared_text.exe` reproduced on untouched Pie master as well as this row; the complete test gate also passed once after formatting. Diagnosis is separate from this capability migration.

## Out of Scope

Changing Eio's already explicit capabilities, redesigning Pie services, deploying or replacing the installed Pie executable, and changes inside Ox.
