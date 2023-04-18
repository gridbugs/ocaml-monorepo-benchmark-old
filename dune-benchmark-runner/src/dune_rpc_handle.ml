type t = {
  chan : Lwt_io.input_channel * Lwt_io.output_channel;
  init : Dune_rpc_private.Initialize.Request.t;
}

let create_exn ~workspace_root =
  let open Lwt.Syntax in
  let init =
    Dune_rpc_private.Initialize.Request.create
      ~id:
        (Dune_rpc_private.Id.make (Csexp.Atom "ocaml-monorepo-benchmark-runner"))
  in
  let build_dir = Printf.sprintf "%s/_build" workspace_root in
  Logs.info (fun m -> m "connecting to RPC server with build dir %s" build_dir);
  let* where_result =
    Dune_rpc_lwt.Private.Where.get ~env:(Fun.const None) ~build_dir
  in
  match where_result with
  | Ok (Some where) ->
      let+ chan = Dune_rpc_lwt.Private.connect_chan where in
      { chan; init }
  | Error _ | Ok None -> failwith "bad where"

let create ~workspace_root = Lwt_result.catch (create_exn ~workspace_root)

let rec create_retrying ~workspace_root =
  let open Lwt.Syntax in
  let* result = create ~workspace_root in
  match result with
  | Ok t -> Lwt.return t
  | Error _ ->
      let* () = Lwt_unix.sleep 0.5 in
      create_retrying ~workspace_root

let connect { chan; init } ~f = Dune_rpc_lwt.Private.Client.connect chan init ~f

let request_exn t request =
  connect t ~f:(fun client ->
      let open Lwt_result.Syntax in
      let open Lwt.Infix in
      let* versioned_request =
        Dune_rpc_lwt.Private.Client.Versioned.prepare_request client request
        >|= Result.map_error Dune_rpc_private.Version_error.message
      in
      Dune_rpc_lwt.Private.Client.request client versioned_request ()
      >|= Result.map_error Dune_rpc_private.Response.Error.message)

let build_count t = request_exn t Dune_rpc_private.Public.Request.build_count

let ping t =
  let open Lwt.Infix in
  request_exn t Dune_rpc_private.Public.Request.ping >|= function
  | Ok () -> ()
  | Error message ->
      print_endline "error from ping";
      print_endline message
