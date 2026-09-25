(** Ordinary dictation resource budgets.

    These are explicit engineering limits, not user-facing tuning flags. *)

val queue_pcm_bytes : int
(** 1 MiB PCM staged per recording queue (Pulse, Ptt_loop, provider). *)

val queue_items : int
(** 1024 staged queue items. Empty PCM is not enqueued; tiny non-empty chunks
    still consume this metadata cap. A byte cap alone does not bound item count.
*)

val capture_pcm_bytes : int
(** 64 MiB total captured PCM for an attempt. *)

val result_utf8_bytes : int
(** 1 MiB retained UTF-8 result text. *)
