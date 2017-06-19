-module(commonjs).

%% API exports
-export ([bundle_single_js/1, bundle_js_in_dir/2]).

%%====================================================================
%% API functions
%%====================================================================
bundle_js_in_dir(Input_dir, Watch_mode) ->
    case Watch_mode of
        true ->
            Pids = bundle_js_in_dir_(Input_dir, true),
            watch(Input_dir, Pids);
        _ ->
            bundle_js_in_dir_(Input_dir, false)
    end.
    
bundle_single_js(Js_entry_file) ->
    commonjs_packager:bundle_single_js(Js_entry_file, false).

%%====================================================================
%% Internal functions
%%====================================================================
bundle_js_in_dir_(Input_dir, Watch_mode) ->
    Js_files = filelib:wildcard(filename:join(Input_dir, filename:join("**", "*.js"))),
    Files = lists:filter(
        fun(Js_file) ->
            (lists:prefix("_", filename:basename(Js_file)) == false) and (lists:suffix("-bundled.js", Js_file) == false)
        end, 
    Js_files),
    bundle_js_entries(Files, [], Watch_mode).

bundle_js_entries([], Acc, _) ->
    Acc;
bundle_js_entries([Js_entry_file|Rest], Acc, Watch_mode) ->
    Pid = spawn(commonjs_packager, bundle_single_js, [Js_entry_file, Watch_mode]),
    bundle_js_entries(Rest, [Pid|Acc], Watch_mode).

watch(Dir, Pids) ->
    io:format("watching ~p ~n", [Dir]),
    spawn(fun()-> 
        fs:start_link(fs_watcher, filename:absname(Dir)),
        fs:subscribe(fs_watcher),
        notify_if_file_changed(Pids)
    end).

notify_if_file_changed(Pids) ->
    receive
        {_Pid, {fs,file_event}, {File, [inodemetamod,modified]}} ->
            io:format("file changed~p ~n", [File]),
            case lists:suffix("-bundled.js", File) of
                true -> do_nothing;
                false ->
                    lists:foreach(
                        fun (Pid) ->
                            Pid ! {file_changed, File}
                        end,
                    Pids)
            end;
        _ -> ignore
    end,
    notify_if_file_changed(Pids).