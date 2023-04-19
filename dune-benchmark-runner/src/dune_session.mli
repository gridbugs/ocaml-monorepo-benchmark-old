type t

val create : dune_exe_path:string -> workspace_root:string -> t
val clean : t -> unit

val build :
  t -> build_target:string -> stdio_redirect:Command.Stdio_redirect.t -> unit

module Trace_file : sig
  type t

  val durations_micros_in_order : t -> int list
end

module Watch_mode : sig
  type t

  val stop : t -> Trace_file.t
  val build_count : t -> int
  val wait_for_nth_build : t -> int -> unit
  val workspace_root : t -> string
end

val watch_mode_start :
  t ->
  build_target:string ->
  stdio_redirect:Command.Stdio_redirect.t ->
  Watch_mode.t
