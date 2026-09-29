---
kind: requirement
---
# Ox integration-gap documentation

## Intent

Keep Pie's deliberate timeout workarounds traceable to concrete gaps in Ox's public APIs so future Ox improvements can replace them. This capability records only gaps that affect Pie's current provider integrations; it is not a general Ox roadmap.

## Requirements

- The Pie Ox integration-gap documentation shall identify that the Ox OpenRouter helpers do not expose response-idle-timeout configuration. ^etagap-jk25
- The Pie Ox integration-gap documentation shall identify the total-effect timeout that Pie uses in place of OpenRouter response-idle-timeout configuration. ^etagap-3uxz
- The Pie Ox integration-gap documentation shall identify that the Ox WebSocket client does not expose an establishment timeout. ^etagap-utb3
- The Pie Ox integration-gap documentation shall identify the total-effect timeout that Pie uses in place of WebSocket establishment-timeout configuration. ^etagap-g1ip

## Verification seams

- Review `docs/missing-ox-api.md` against the current Ox OpenRouter and WebSocket client interfaces and the corresponding Pie timeout call sites.
