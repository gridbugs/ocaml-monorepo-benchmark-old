type t

val create_retrying : workspace_root:string -> t
val ping : t -> unit
val build_count : t -> int
