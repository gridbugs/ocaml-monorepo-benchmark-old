let init =
  Dune_rpc_private.Initialize.Request.create
    ~id:
      (Dune_rpc_private.Id.make (Csexp.Atom "ocaml-monorepo-benchmark-runner"))

module Where = struct
  let of_workspace_root workspace_root =
    let build_dir = Printf.sprintf "%s/_build" workspace_root in
    Dune_rpc_lwt.Private.Where.default ~build_dir ()
end

module Chan = struct
  let try_connect where =
    Lwt_result.catch (Dune_rpc_lwt.Private.connect_chan where)

  let rec connect_retrying where =
    let open Lwt.Syntax in
    let* result = try_connect where in
    match result with
    | Ok chan ->
        Logs.info (fun m ->
            m "connected to RPC server (%s)"
              (Dune_rpc_private.Where.to_string where));
        Lwt.return chan
    | Error (e : exn) ->
        Logs.warn (fun m ->
            m "failed to connect to RPC server (%s): %s (retrying)"
              (Dune_rpc_private.Where.to_string where)
              (Printexc.to_string e));
        let* () = Lwt_unix.sleep 0.5 in
        connect_retrying where
end

type t = Dune_rpc_lwt.Private.Client.t

let with_client ~workspace_root ~f =
  let open Lwt.Syntax in
  let* chan = Chan.connect_retrying (Where.of_workspace_root workspace_root) in
  Dune_rpc_lwt.Private.Client.connect chan init ~f

let request t request =
  let open Lwt_result.Syntax in
  let open Lwt.Infix in
  let* versioned_request =
    Dune_rpc_lwt.Private.Client.Versioned.prepare_request t request
    >|= Result.map_error (fun e -> Dune_rpc_private.Version_error.E e)
  in
  Dune_rpc_lwt.Private.Client.request t versioned_request ()
  >|= Result.map_error (fun e -> Dune_rpc_private.Response.Error.E e)

let ping t =
  request t Dune_rpc_private.Public.Request.ping |> Lwt_result.get_exn

let build_count t =
  request t Dune_rpc_private.Public.Request.build_count |> Lwt_result.get_exn
