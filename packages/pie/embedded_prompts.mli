(** Pass-2 prompts compiled from [config/default_prompts].

    A missing [correction.md] or [correction-translate.md] in the user's prompts
    directory falls back to these strings. A binding that names its own file
    does not. *)

val correction : string
(** Embedded [correction.md], used by transcribe bindings. *)

val correction_translate : string
(** Embedded [correction-translate.md], used by translate bindings. *)
