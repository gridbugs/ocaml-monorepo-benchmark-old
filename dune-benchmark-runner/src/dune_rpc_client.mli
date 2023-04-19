type t

val with_client : workspace_root:string -> f:(t -> 'a Lwt.t) -> 'a Lwt.t
val ping : t -> unit Lwt.t
val build_count : t -> int Lwt.t
