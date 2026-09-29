---
kind: requirement
---
# Speech operation deadlines

## Intent

Bound finite external speech-service operations so a stalled provider cannot hang Pie, while leaving established realtime sessions free to remain active indefinitely. This capability covers timeout policy at Pie's provider boundary, not transport-level timeout APIs supplied by Ox.

## Requirements

- While Pie refreshes an OAuth token, Pie shall apply a 30-second response-idle timeout to the token-refresh response. ^stttime-33v8
- While Pie establishes a Codex Realtime WebSocket, Pie shall apply a ten-second total timeout to WebSocket establishment. ^stttime-nby7
- While a realtime WebSocket session is established, Pie shall exempt the active stream from response-idle and total-session deadlines. ^stttime-i0cx
- While Pie performs direct OpenRouter transcription, Pie shall apply a 60-second total timeout to the transcription operation. ^stttime-blym
- While Pie performs OpenRouter response-based transcription or translation, Pie shall apply a 120-second total timeout to the response operation. ^stttime-pf5a
- If a finite speech-service operation exceeds its Pie timeout, then Pie shall report the timeout through the operation's typed error channel. ^stttime-lhfu

## Verification seams

- Use stalled local HTTP and WebSocket test servers at the provider boundary and a controllable Ox clock to observe each finite deadline without waiting for wall-clock time.
- Exercise an established WebSocket stream beyond the finite-operation deadlines and observe that Pie leaves the stream active.
- Exercise each timeout through the command-facing STT functions and assert the typed failure exposed to callers.
