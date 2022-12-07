open Switch_builder

let cached_repo_with_overlay () =
  let opam_repo = Repository.path "./data/repos/opam-repository" in
  let opam_overlays = Repository.path "./data/repos/opam-overlays" in
  let combined_repo = Repository.combine opam_repo opam_overlays in
  let cache = Repository.Cache.create () in
  let cached_repo = Repository.cache cache combined_repo in
  cached_repo

let depends_on_dune opam =
  let deps = OpamFile.OPAM.depends opam in
  let dep_name_list =
    OpamFormula.fold_left (fun xs (x, _) -> x :: xs) [] deps
  in
  List.exists
    (fun p ->
      let s = OpamPackage.Name.to_string p in
      String.equal s "dune")
    dep_name_list

let has_no_build_commands opam =
  match OpamFile.OPAM.build opam with [] -> true | _ -> false

let mkenv package =
  Env.common
  |> Env.extend "os" (OpamVariable.S "linux")
  |> Env.extend "os-version" (OpamVariable.S "22.04")
  |> Env.extend "os-distribution" (OpamVariable.S "ubuntu")
  |> Env.extend "ocaml-system:version" (OpamVariable.S "4.14.0")
  |> Env.extend "ocaml-base-compiler:version" (OpamVariable.S "4.14.0")
  |> Env.extend "ocaml-variants:version" (OpamVariable.S "4.14.0")
  |> Env.extend "version"
       (OpamVariable.S
          (OpamPackage.version package |> OpamPackage.Version.to_string))

let check_conflict opam ~packages_by_name =
  let package = OpamFile.OPAM.package opam in
  let env = mkenv package in
  let conflicts_formula =
    OpamFile.OPAM.conflicts opam |> OpamFilter.filter_formula env
  in
  let res =
    Eval.m
      (fun (name, version_formula) ->
        match OpamPackage.Name.Map.find_opt name packages_by_name with
        | None -> Ok ()
        | Some pkg_in_set ->
            let version = OpamPackage.version pkg_in_set in
            let version_matches_formula =
              OpamFormula.check_version_formula version_formula version
            in
            if version_matches_formula then Error pkg_in_set else Ok ())
      conflicts_formula
  in
  match res with Ok () -> None | Error p -> Some p

let find_conflict packages ~repo =
  let packages_by_name =
    packages
    |> List.map (fun pkg -> (OpamPackage.name pkg, pkg))
    |> OpamPackage.Name.Map.of_list
  in
  List.find_opt
    (fun package ->
      let opam = Repository.read_opam repo package in
      check_conflict opam ~packages_by_name |> Option.is_some)
    packages

let package_incompatible opam ~packages_by_name ~assumed_deps =
  let package = OpamFile.OPAM.package opam in
  if
    OpamPackage.name package |> OpamPackage.Name.to_string
    |> String.equal "ocaml"
  then None
  else
    let env = mkenv package in
    let deps = OpamFile.OPAM.depends opam |> OpamFilter.filter_formula env in
    let res =
      Eval.m
        (fun (name, version_formula) ->
          if OpamPackage.Name.Set.mem name assumed_deps then Ok ()
          else
            match OpamPackage.Name.Map.find_opt name packages_by_name with
            | None -> Error (`Missing (name, version_formula))
            | Some dep ->
                let dep_version = OpamPackage.version dep in
                let has_compatible_dep =
                  OpamFormula.check_version_formula version_formula dep_version
                in
                if has_compatible_dep then Ok ()
                else Error (`Incompatible_version (name, version_formula)))
        deps
    in
    match res with Ok () -> None | Error e -> Some e

type unmep_dependencies = {
  had_missing_deps : (OpamPackage.t * OpamPackage.Name.t) list;
  had_incompatible_version_deps : (OpamPackage.t * OpamPackage.Name.t) list;
}

let remove_unmet_dependencies packages ~repo ~assumed_deps =
  let packages_by_name =
    packages
    |> List.map (fun pkg -> (OpamPackage.name pkg, pkg))
    |> OpamPackage.Name.Map.of_list
  in
  let init =
    ( OpamPackage.Set.of_list packages,
      { had_missing_deps = []; had_incompatible_version_deps = [] } )
  in
  List.fold_left
    (fun ((packages, { had_missing_deps; had_incompatible_version_deps }) as
         acc) package ->
      let opam = Repository.read_opam repo package in
      match package_incompatible opam ~packages_by_name ~assumed_deps with
      | None -> acc
      | Some (`Missing (missing_dep, _)) ->
          ( OpamPackage.Set.remove package packages,
            {
              had_missing_deps = (package, missing_dep) :: had_missing_deps;
              had_incompatible_version_deps;
            } )
      | Some (`Incompatible_version (missing_dep, _)) ->
          ( OpamPackage.Set.remove package packages,
            {
              had_missing_deps;
              had_incompatible_version_deps =
                (package, missing_dep) :: had_incompatible_version_deps;
            } ))
    init packages

let pkg_to_filtered_formula pkg : OpamTypes.filtered_formula =
  let name = OpamPackage.name pkg in
  let version = OpamPackage.version pkg in
  Atom
    ( name,
      Atom (Constraint (`Eq, FString (OpamPackage.Version.to_string version)))
    )

let pkg_set_to_filtered_formula pkg_set : OpamTypes.filtered_formula =
  OpamPackage.Set.elements pkg_set
  |> List.map pkg_to_filtered_formula
  |> OpamFormula.ands

let pkg_set_to_opam_file pkg_set =
  OpamFile.OPAM.empty
  |> OpamFile.OPAM.with_depends (pkg_set_to_filtered_formula pkg_set)

let write_string_file string ~path =
  let f =
    Unix.openfile path [ Unix.O_RDWR; Unix.O_TRUNC; Unix.O_CREAT ] 0o666
  in
  let _ = Unix.write_substring f string 0 (String.length string) in
  Unix.close f

let write_opam_file opam ~path =
  let opam_file_string = OpamFile.OPAM.write_to_string opam in
  write_string_file opam_file_string ~path

let is_available opam ~arch =
  let package = OpamFile.OPAM.package opam in
  let available = OpamFile.OPAM.available opam in
  let env = mkenv package |> Env.extend "arch" (OpamVariable.S arch) in
  OpamFilter.eval_to_bool env available

module Packages = struct
  open Sexplib.Std
  module Sexp = Sexplib.Sexp

  module Package = struct
    type t = OpamPackage.t

    let t_of_sexp = function
      | Sexp.Atom atom -> OpamPackage.of_string atom
      | other ->
          failwith (Printf.sprintf "failed to parse: %s" (Sexp.to_string other))
  end

  type t = Package.t list [@@deriving of_sexp]

  let to_string_pretty t =
    Printf.sprintf "(\n %s\n)"
      (List.map OpamPackage.to_string t |> String.concat "\n ")

  let of_string s = Sexp.of_string s |> t_of_sexp
end
