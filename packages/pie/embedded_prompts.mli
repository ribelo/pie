(** Prompts compiled from [config/default_prompts].

    A missing default file in the user's prompts directory falls back to these
    strings. A binding that names its own file does not. *)

val correction : string
(** Embedded [correction.md], used by two-pass transcribe bindings. *)

val correction_translate : string
(** Embedded [correction-translate.md], used by two-pass translate bindings. *)

val one_pass : string
(** Embedded [one-pass.md], used by one-pass transcribe bindings with
    correction. *)

val one_pass_verbatim : string
(** Embedded [one-pass-verbatim.md], used by one-pass transcribe bindings with
    [correct = false]. *)

val one_pass_translate : string
(** Embedded [one-pass-translate.md], used by one-pass translate bindings. *)
