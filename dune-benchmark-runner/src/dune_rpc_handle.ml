type t = {
  chan : Lwt_io.input_channel * Lwt_io.output_channel;
  init : Dune_rpc_private.Initialize.Request.t;
}

module Async = struct
  let create_exn ~workspace_root =
    let open Lwt.Syntax in
    let init =
      Dune_rpc_private.Initialize.Request.create
        ~id:
          (Dune_rpc_private.Id.make
             (Csexp.Atom "ocaml-monorepo-benchmark-runner"))
    in
    let build_dir = Printf.sprintf "%s/_build" workspace_root in
    let where = Dune_rpc_lwt.Private.Where.default ~build_dir () in
    Logs.info (fun m ->
        m "connecting to RPC server at: %s"
          (Dune_rpc_private.Where.to_string where));
    let+ chan = Dune_rpc_lwt.Private.connect_chan where in
    { chan; init }

  let create ~workspace_root = Lwt_result.catch (create_exn ~workspace_root)

  let rec create_retrying ~workspace_root =
    let open Lwt.Syntax in
    let* result = create ~workspace_root in
    match result with
    | Ok t -> Lwt.return t
    | Error _ ->
        let* () = Lwt_unix.sleep 0.5 in
        create_retrying ~workspace_root

  let connect { chan; init } ~f =
    Dune_rpc_lwt.Private.Client.connect chan init ~f

  let request t request =
    connect t ~f:(fun client ->
        let open Lwt_result.Syntax in
        let open Lwt.Infix in
        let* versioned_request =
          Dune_rpc_lwt.Private.Client.Versioned.prepare_request client request
          >|= Result.map_error Dune_rpc_private.Version_error.message
        in
        Dune_rpc_lwt.Private.Client.request client versioned_request ()
        >|= Result.map_error Dune_rpc_private.Response.Error.message)
end

let create_retrying ~workspace_root =
  Lwt_main.run (Async.create_retrying ~workspace_root)

let run_exn lwt = Lwt_main.run lwt |> Result.get_ok

let build_count t =
  Async.request t Dune_rpc_private.Public.Request.build_count |> run_exn

let ping t = Async.request t Dune_rpc_private.Public.Request.ping |> run_exn
