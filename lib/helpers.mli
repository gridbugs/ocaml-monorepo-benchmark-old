open Switch_builder

val cached_repo_with_overlay : unit -> Repository.t
val depends_on_dune : OpamFile.OPAM.t -> bool
val has_no_build_commands : OpamFile.OPAM.t -> bool
val is_available : OpamFile.OPAM.t -> arch:string -> bool
val mkenv : OpamPackage.t -> OpamFilter.env

val find_conflict :
  OpamPackage.t list -> repo:Repository.t -> OpamPackage.t option

type unmep_dependencies = {
  had_missing_deps : (OpamPackage.t * OpamPackage.Name.t) list;
  had_incompatible_version_deps : (OpamPackage.t * OpamPackage.Name.t) list;
}

val remove_unmet_dependencies :
  OpamPackage.t list ->
  repo:Repository.t ->
  assumed_deps:OpamPackage.Name.Set.t ->
  OpamPackage.Set.t * unmep_dependencies

val pkg_set_to_opam_file : OpamPackage.Set.t -> OpamFile.OPAM.t
val write_opam_file : OpamFile.OPAM.t -> path:string -> unit
val write_string_file : string -> path:string -> unit

module Packages : sig
  type t = OpamPackage.t list

  val of_string : string -> t
  val to_string_pretty : t -> string
end
