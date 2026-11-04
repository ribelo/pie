---
status: ready-for-agent
category: enhancement
---

# Spike: wayland library and layer-shell overlay under OxCaml

Spec: [[01-screen-capture]]

**Covers:** scap-1nk0

Build ocaml-wayland 2.3 with xmlm and logs against the 5.4.0+ox switch in an ignored scratch directory, without installing into the shared switch. Generate bindings for wlr-layer-shell-unstable-v1 and use wl_shm to show a transparent layer surface with a red edge on niri for two seconds. Record the result under Comments. If it cannot work, report it to Rafał: the feature is then dropped.
