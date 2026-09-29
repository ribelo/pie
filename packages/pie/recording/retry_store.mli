type mode = Transcribe | Translate

module Active : sig
  type generation
  type store
  type writer

  type metadata =
    | Audio of { mode : mode; pcm_bytes : int; generation : generation }
    | Text of {
        mode : mode;
        utf8_bytes : int;
        dispatch_uncertain : bool;
        generation : generation;
      }

  val generation_equal : generation -> generation -> bool

  val open_store :
    caps:< Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.time ; .. > ->
    root:string ->
    (store, [> `Retry_store of string ]) result

  val open_audio_writer :
    store -> mode:mode -> (writer, [> `Retry_store of string ]) result

  val append_audio :
    writer -> bytes -> (unit, [> `Retry_store of string ]) result

  val close_writer : writer -> (unit, [> `Retry_store of string ]) result
  val abort_writer : writer -> (unit, [> `Retry_store of string ]) result

  val publish_audio :
    store -> writer -> (generation, [> `Retry_store of string ]) result

  val publish_text :
    store ->
    mode:mode ->
    text:string ->
    dispatch_uncertain:bool ->
    (generation, [> `Retry_store of string ]) result

  val peek : store -> (metadata option, [> `Retry_store of string ]) result

  val load_audio :
    store -> generation -> (bytes, [> `Retry_store of string ]) result

  val load_text :
    store -> generation -> (string, [> `Retry_store of string ]) result

  val clear : store -> generation -> (unit, [> `Retry_store of string ]) result

  module Test_hooks : sig
    val with_fchmod_fail : (unit -> 'a) -> 'a
    val with_close_fail : (unit -> 'a) -> 'a
    val take_reused_fd : unit -> Unix.file_descr option
    val with_unlink_fail : (unit -> 'a) -> 'a
    val with_emfile : (unit -> 'a) -> 'a
    val with_index_open_fail : (unit -> 'a) -> 'a
    val with_fsync_fail : (unit -> 'a) -> 'a
    val with_close_and_unlink_fail : (unit -> 'a) -> 'a
  end
end
