-module(commonjs).

%% API exports
-export ([bundle_single_js/2, bundle_js_in_dir/3]).

%%====================================================================
%% API functions
%%====================================================================
bundle_js_in_dir(Input_dir, Output_dir, Watch_mode) ->
    case Watch_mode of
        true ->
            Pids = bundle_js_in_dir_(Input_dir, Output_dir, true),
            watch(Input_dir, Pids, Input_dir, Output_dir);
        _ ->
            bundle_js_in_dir_(Input_dir, Output_dir, false)
    end.
    
bundle_single_js(Js_entry_file, Output) ->
    commonjs_packager:bundle_single_js(Js_entry_file, Output, false).

%%====================================================================
%% Internal functions
%%====================================================================
bundle_js_in_dir_(Input_dir, Output_dir, Watch_mode) ->
    Js_files = filelib:wildcard(filename:join(Input_dir, filename:join("**", "*.js"))),
    Files = lists:filter(
        fun(Js_file) ->
            not lists:prefix("_", filename:basename(Js_file))
        end, 
    Js_files),
    bundle_js_entries(Files, [], Watch_mode, Input_dir, Output_dir).

bundle_js_entries([], Pids, _, _Input_dir, _Output_dir) ->
    Pids;
bundle_js_entries([Js_entry_file|Rest], Acc, Watch_mode, Input_dir, Output_dir) ->
    Output_file = filename:join(Output_dir, re:replace(filename:absname(Js_entry_file), filename:absname(Input_dir) ++ "/", "", [{return,list}])),
    Pid = spawn(commonjs_packager, bundle_single_js, [Js_entry_file, Output_file, Watch_mode]),
    bundle_js_entries(Rest, [Pid|Acc], Watch_mode, Input_dir, Output_dir).

watch(Dir, Pids, Input_dir, Output_dir) ->
    io:format("watching ~p ~n", [Dir]),
    spawn(fun()-> 
        fs:start_link(fs_watcher, filename:absname(Dir)),
        fs:subscribe(fs_watcher),
        notify_if_file_changed(Pids, Input_dir, Output_dir)
    end).

notify_if_file_changed(Pids, Input_dir, Output_dir) ->
    receive
        {_Pid, {fs,file_event}, {File, Events}} ->
            case (lists:member(modified, Events) orelse lists:member(renamed, Events)) andalso lists:suffix(".js", File) of
                false -> do_nothing;
                true ->
                    lists:foreach(
                        fun (Pid) ->
                            Pid ! {file_changed, File, Input_dir, Output_dir}
                        end,
                    Pids)
            end;
        _ -> ignore
    end,
    notify_if_file_changed(Pids, Input_dir, Output_dir).