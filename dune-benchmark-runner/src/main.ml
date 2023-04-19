let () =
  let {
    Cli.dune_exe_path;
    build_target;
    monorepo_path;
    skip_clean;
    skip_one_shot;
    print_dune_output;
  } =
    Cli.parse ()
  in
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let stdio_redirect = if print_dune_output then `This_process else `Ignore in
  let dune_session =
    Dune_session.create ~dune_exe_path ~workspace_root:monorepo_path
  in
  if not skip_clean then (
    Logs.info (fun m -> m "Cleaning");
    Dune_session.clean dune_session);
  let measure_one_shot_build name =
    let duration_secs =
      Timing.measure_secs (fun () ->
          Dune_session.build dune_session ~build_target ~stdio_redirect)
    in
    { Benchmark_result.name; duration_secs }
  in
  let one_shot_benchmark_results =
    if skip_one_shot then []
    else (
      Logs.info (fun m -> m "Building from scratch");
      let build_from_scratch_benchmark_result =
        measure_one_shot_build "build from scratch"
      in
      Logs.info (fun m -> m "Rebuilding after making no changes");
      let null_build_benchmark_result = measure_one_shot_build "null build" in
      [ build_from_scratch_benchmark_result; null_build_benchmark_result ])
  in
  let dune_watch_mode =
    Dune_session.watch_mode_start dune_session ~build_target ~stdio_redirect
  in
  let scenario_runner = Scenario_runner.create ~monorepo_path in
  Scenario_runner.run_watch_mode_scenarios ~dune_watch_mode scenario_runner;
  let trace_file = Dune_session.Watch_mode.stop dune_watch_mode in
  Scenario_runner.undo_all_changes scenario_runner;
  let watch_mode_benchmark_results =
    Dune_session.Trace_file.durations_micros_in_order trace_file
    |> Scenario_runner.convert_durations_into_benchmark_results scenario_runner
  in
  let benchmark_results =
    one_shot_benchmark_results @ watch_mode_benchmark_results
  in
  print_endline
    (Yojson.pretty_to_string
       (Benchmark_result.list_to_current_bench_json benchmark_results))
