---
status: done
category: enhancement
---

# Spike: wayland library and layer-shell overlay under OxCaml

Spec: [[01-screen-capture]]

**Covers:** scap-1nk0

Build ocaml-wayland 2.3 with xmlm and logs against the 5.4.0+ox switch in an ignored scratch directory, without installing into the shared switch. Generate bindings for wlr-layer-shell-unstable-v1 and use wl_shm to show a transparent layer surface with a red edge on niri for two seconds. Record the result under Comments. If it cannot work, report it to Rafał: the feature is then dropped.
## Comments

### 2026-11-04 result: works

- ocaml-wayland at f2cec05 (release 2.3), xmlm and logs from their upstream heads build with Dune against the `5.4.0+ox` switch (eio 1.6, cstruct 6.2.0, fmt 0.11.0) in `.scratch/wayland-spike/`, without installing anything. xmlm and logs have no Dune build upstream (topkg); a one-line `dune` file each was enough. The dev profile fails on upstream warnings in xmlm and logs; the release profile builds. ocaml-wayland raises only the OxCaml `unsafe_multidomain` alert for `Unix.putenv` in `unix_transport.ml`.
- The generated client for `wlr-layer-shell-unstable-v1` ships with `wayland.protocols`; no extra protocol file is needed.
- A spike client bound `zwlr_layer_shell_v1` v4 on niri 26.04, created an overlay-layer surface anchored to all edges with exclusive zone -1, no keyboard interactivity and an empty input region, got `configure 1920x1080` (logical size of the 4K output), attached an ARGB8888 `wl_shm` buffer (transparent with a 6 px red edge), kept it for 2 s and exited cleanly.
- The agent sandbox denies `/dev/shm`, so the spike put the shm file in the scratch directory. `grim` fails in the sandbox for the same reason, so the frame was not checked on a screenshot. `pie.service` is not sandboxed this way.
- Open for ticket 05: how xmlm, logs and wayland enter the shared switch and the flake. That changes the build and needs Rafał's word.
