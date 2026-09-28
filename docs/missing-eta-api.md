# Missing Eta APIs used by Pie

Pie applies total-effect deadlines where Eta's provider APIs do not expose the
more precise transport deadline that Pie needs. These workarounds should be
removed when the corresponding Eta APIs become available.

## OpenRouter response idle timeout

`Eta_ai_openrouter.responses` and `Eta_ai_openrouter.transcription` construct
their HTTP requests internally and do not expose
`Eta_http.Request.Response_idle_timeout.t`. Pie therefore cannot configure a
response-progress deadline for those requests. (`etagap-jk25`)

Pie currently wraps direct transcription in a 60-second total
`Eta.Effect.timeout_as` and response-based transcription or translation in a
120-second total timeout. A future Eta parameter should replace these broader
deadlines with response-idle policy where appropriate. (`etagap-3uxz`)

## WebSocket establishment timeout

`Eta_http_eio.Ws.Client.connect` does not accept an establishment timeout for
the TCP, TLS, and HTTP upgrade sequence. (`etagap-utb3`)

Pie currently wraps Codex Realtime WebSocket connection in a ten-second total
`Eta.Effect.timeout_as`. The established WebSocket stream is deliberately not
wrapped in that deadline. A future Eta connection-timeout parameter should
replace the total-effect establishment workaround. (`etagap-g1ip`)
