val iso639_1 : string -> string option
(** [iso639_1 name] is the ISO-639-1 code for a configured language name.

    English names Polish, English, German, French, Spanish, Italian, Portuguese,
    Ukrainian, Russian, Czech, Dutch, Japanese and Chinese map to their codes.
    Matching ignores case and surrounding space. A two-letter code passes
    through lowercased. Anything else is [None], and the caller then omits the
    language field. *)
