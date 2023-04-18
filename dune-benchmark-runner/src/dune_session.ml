type t = { dune_exe_path : string; workspace_root : string }

let create ~dune_exe_path ~workspace_root = { dune_exe_path; workspace_root }

let make_command t args =
  let all_args = List.append args [ "--root"; t.workspace_root ] in
  Command.create t.dune_exe_path all_args

let run_command_exn t args ~stdio_redirect =
  let command = make_command t args in
  match Command.run_blocking_exn ~stdio_redirect command with
  | 0 -> ()
  | other ->
      failwith
        (Printf.sprintf "command `%s` exited with unexpected status: %d"
           (Command.to_string command)
           other)

let clean t = run_command_exn t [ "clean" ] ~stdio_redirect:`Ignore

let build t ~build_target ~stdio_redirect =
  run_command_exn t [ "build"; build_target ] ~stdio_redirect

let internal_build_count t =
  let command = make_command t [ "internal"; "build-count" ] in
  let output =
    Command.run_blocking_stdout_string command
    |> String.split_on_char '\n' |> List.hd
  in
  match int_of_string_opt output with
  | Some build_count -> build_count
  | None ->
      failwith
        (Printf.sprintf
           "Unexpected output of `%s`. Expected an int, got \"%s\"."
           (Command.to_string command)
           output)

let wait_for_rpc_server t =
  let ping_command = make_command t [ "rpc"; "ping" ] in
  let delay_s = 0.5 in
  let rec loop () =
    if Command.run_blocking_exn ping_command ~stdio_redirect:`Ignore <> 0 then (
      Unix.sleepf delay_s;
      loop ())
  in
  loop ()

let wait_for_nth_build t n =
  let delay_s = 0.5 in
  let rec loop () =
    if internal_build_count t < n then (
      Unix.sleepf delay_s;
      loop ())
  in
  loop ()

module Trace_file = struct
  type t = { path : string }

  let random () =
    Random.self_init ();
    let i = Random.int 10000 in
    let path = Printf.sprintf "/tmp/dune-trace-file.%04d.json" i in
    { path }

  let parse { path } = Yojson.Safe.from_file path

  let yojson_to_durations_micros_in_order yojson =
    let eager_watch_mode_build_complete_event_name =
      "eager_watch_mode_build_complete"
    in
    match yojson with
    | `List entries ->
        List.filter_map
          (function
            | `Assoc assoc ->
                Option.bind (List.assoc_opt "name" assoc) (function
                  | `String event_name
                    when String.equal event_name
                           eager_watch_mode_build_complete_event_name ->
                      Option.bind (List.assoc_opt "dur" assoc) (function
                        | `Int dur -> Some dur
                        | _ -> None)
                  | _ -> None)
            | _ -> failwith "unexpected structure")
          entries
    | _ -> failwith "unexpected  structure"

  let durations_micros_in_order t =
    parse t |> yojson_to_durations_micros_in_order
end

module Watch_mode = struct
  type nonrec t = {
    running : Command.Running.t;
    dune_session : t;
    trace_file : Trace_file.t;
  }

  let stop { running; trace_file; _ } =
    Logs.info (fun m -> m "stopping watch mode");
    Command.Running.term running;
    trace_file

  let build_count { dune_session; _ } = internal_build_count dune_session

  let wait_for_nth_build { dune_session; _ } n =
    wait_for_nth_build dune_session n

  let workspace_root { dune_session; _ } = dune_session.workspace_root
end

let watch_mode_start t ~build_target ~stdio_redirect =
  let trace_file = Trace_file.random () in
  Logs.info (fun m -> m "starting dune in watch mode");
  Logs.info (fun m -> m "will store trace in %s" trace_file.path);
  let running =
    make_command t
      [
        "build";
        build_target;
        "-j";
        "auto";
        "--watch";
        "--trace-file";
        trace_file.path;
      ]
    |> Command.run_background ~stdio_redirect
  in
  Logs.info (fun m -> m "waiting for rpc server to start");
  wait_for_rpc_server t;
  Logs.info (fun m -> m "waiting for initial build to finish");
  wait_for_nth_build t 1;
  { Watch_mode.running; dune_session = t; trace_file }

module Watch_mode_ = struct
  type nonrec t = {
    running : Command.Running.t;
    dune_session : t;
    trace_file : Trace_file.t;
    rpc_handle : Dune_rpc_handle.t;
  }

  let stop { running; trace_file; _ } =
    Logs.info (fun m -> m "stopping watch mode");
    Command.Running.term running;
    trace_file
end

let watch_mode_start_ t ~build_target ~stdio_redirect =
  let open Lwt.Syntax in
  let trace_file = Trace_file.random () in
  Logs.info (fun m -> m "starting dune in watch mode");
  Logs.info (fun m -> m "will store trace in %s" trace_file.path);
  let running =
    make_command t
      [
        "build";
        build_target;
        "-j";
        "auto";
        "--watch";
        "--trace-file";
        trace_file.path;
      ]
    |> Command.run_background ~stdio_redirect
  in
  let* rpc_handle =
    Dune_rpc_handle.create_retrying ~workspace_root:t.workspace_root
  in
  print_endline "before ping";
  let* () = Lwt_unix.sleep 2.0 in
  print_endline "x";
  let+ () = Dune_rpc_handle.ping rpc_handle in
  let () = failwith "after ping" in
  { Watch_mode_.running; dune_session = t; trace_file; rpc_handle }
