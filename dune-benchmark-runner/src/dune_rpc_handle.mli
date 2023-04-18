type t

val create_retrying : workspace_root:string -> t Lwt.t
val ping : t -> unit Lwt.t
