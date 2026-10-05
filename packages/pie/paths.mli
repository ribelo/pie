val xdg_data_home : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val xdg_config_home : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val xdg_runtime_dir : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val data_dir : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val config_dir : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val runtime_dir : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val daemon_socket_path : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val recording_state_path : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val failed_recordings_dir : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val stt_config_path : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Ox_app.Config], [Filename] and [Stdlib]. Cancellation: not a
    cancellation point in Pie; native and helper callback behavior depends on
    [Ox_caps], [Ox_app.Config], [Filename] and [Stdlib]. Finalizers: creates no
    owned resource and registers no release hook. Locking: no lock is acquired.
*)

val prompts_dir : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and path construction implementation depends on
    [Ox_caps], [Filename] and [Stdlib]. Cancellation: not a cancellation point
    in Pie; native and helper callback behavior depends on [Ox_caps], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)
