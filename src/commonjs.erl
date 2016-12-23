-module(commonjs).

%% API exports
-export ([bundle_single_js/1, bundle_js_in_dir/2]).
-export([start_server/1, stop_server/0]).

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
    put("entry_name", Js_entry_file),
    case get("source_cache") =:= undefined of
        true ->
            put("source_cache", #{});
        false -> do_nothing
    end,
    case get("dependency_graph") =:= undefined of
        true ->
            put("dependency_graph", #{});
        false -> do_nothing
    end,
    bundle(Js_entry_file, ""),
    write_bundled_file(Js_entry_file).

start_server(Dir) ->
    bundle_js_in_dir(Dir, true),
    misultin:start_link([
        {port, 8020},
        {static, Dir},
        {loop, fun(Req) -> handle_http(Req) end},
        {ws_loop, fun(Ws) -> handle_websocket(Ws) end}
    ]).

stop_server() ->
    misultin:stop().

%%====================================================================
%% Internal functions
%%====================================================================
handle_http(Req) ->
    Req:ok("Not a static file request.").

handle_websocket(Ws) ->
    Table = ets:new(web_socket_table, [public, named_table]),
    ets:insert(Table, {web_socket, Ws}),
    receive
        {browser, Data} ->
            Ws:send(["received '", Data, "'"]),
            handle_websocket(Ws);
        _Ignore ->
            handle_websocket(Ws)
    end.

get_bundled_content(Js_entry_file) ->
    iolist_to_binary(["(function(){\n", 
        js_require_function(), 
        "\nrequire.sourceCache = ", 
        jsx:prettify(jsx:encode(get("source_cache"))), 
        ";\n",
        maps:get(list_to_binary(Js_entry_file), get("source_cache")),
        ";\n})();"]).

write_bundled_file(Js_entry_file) ->
    Bundled_content = get_bundled_content(Js_entry_file),
    io:format("~p bundled to:~p ~n", [Js_entry_file, Js_entry_file ++ "-bundled.js"]),
    file:write_file(Js_entry_file ++ "-bundled.js", Bundled_content).

rebuild_entry_if_module_changed() ->
    receive
        {file_changed, File} ->
            io:format("file_changed ~p ~n", [File]),
            lists:foreach(
                fun(Module_name) -> 
                    case string_contains(File, binary_to_list(filename:join(Module_name, ""))) of
                        true ->
                            Required_module = binary_to_list(Module_name),
                            case lists:suffix(".js", Required_module) of
                                true ->
                                    bundle(Required_module, "");
                                false -> 
                                    bundle(Required_module, ".js")
                            end,
                            Entry = get("entry_name"),
                            case ets:info(web_socket_table) of
                                undefined -> do_nothing;
                                _ ->
                                    case ets:lookup(web_socket_table, web_socket) of
                                        [{web_socket,Misultin_ws}] ->
                                            Misultin_ws:send([get_bundled_content(Entry)]);
                                        _ -> do_nothing
                                    end
                            end,
                            write_bundled_file(Entry);
                        false -> do_nothing
                    end
                end, 
            maps:keys(get("source_cache")))
    end,
    rebuild_entry_if_module_changed().

string_contains(Big, Small)->
    string:str(Big, Small) > 0.

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
    Pid = spawn(fun() -> 
        bundle_single_js(Js_entry_file),
        case Watch_mode of
            true ->
                rebuild_entry_if_module_changed();
            _ -> do_nothing
        end
    end),
    bundle_js_entries(Rest, [Pid|Acc], Watch_mode).

watch(Dir, Pids) ->
    io:format("watching ~p ~n", [Dir]),
    spawn(fun()-> 
        fs:start_link(fs_watcher, filename:absname(Dir)),
        fs:subscribe(fs_watcher),
        on_file_changed(Pids)
    end).

on_file_changed(Pids) ->
    receive
        {_Pid, {fs,file_event}, {File, [inodemetamod,modified]}} ->
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
    on_file_changed(Pids).

bundle(Js_entry_file, Ext_name) ->
    case file:read_file(Js_entry_file ++ Ext_name) of
        {ok, Content} ->
            Require_regexp = "(require\\((['|\"])(.*?)\\g2\\);?)",
            Replaced = re:replace(remove_comments(Content), Require_regexp, ["require('", filename:join(filename:dirname(Js_entry_file), "\\g3"), "')"], [global]),
            Map = get("source_cache"),
            put("source_cache", Map#{list_to_binary(Js_entry_file) => iolist_to_binary([Replaced, "\n//# sourceURL=", Js_entry_file])}),
            Dependency_map = get("dependency_graph"),
            case re:run(Replaced, Require_regexp, [global,{capture,[3],list}]) of
                {match, Matched} ->
                    Old_dependencies = maps:get(Js_entry_file, Dependency_map, []),
                    Current_dependencies = [Module || [Module] <- Matched],
                    case Current_dependencies =:= Old_dependencies of
                        true -> do_nothing;
                        false ->
                            put("dependency_graph", Dependency_map#{Js_entry_file => Current_dependencies}),
                            lists:foreach(
                                fun(Required_js_path) -> 
                                    case lists:suffix(".js", Required_js_path) of
                                        true ->
                                            bundle(Required_js_path, "");
                                        false -> 
                                            bundle(Required_js_path, ".js")
                                    end
                                end
                            , Current_dependencies)
                    end;
                nomatch -> 
                    put("dependency_graph", Dependency_map#{Js_entry_file => []})
            end;
        {error, enoent} -> 
            error_log(Js_entry_file ++ " is missing")
    end.

error_log(Msg) ->
    io:format("\033[91m ~p \033[0m~n",[Msg]).

remove_comments(Content) ->
    Replaced = re:replace(Content, "//.*", <<"">>, [global]),

    Regexp_block_comment = "\\/\\*[^*]*\\*+([^\\/*][^*]*\\*+)*\\/",
    re:replace(Replaced, Regexp_block_comment, <<" ">>, [global]).

js_require_function() ->
    %% see https://github.com/marijnh/Eloquent-JavaScript/blob/master/10_modules.txt#L465
    <<"
    function require(url){
        if (!require.cache[url]) {
            var source = require.sourceCache[url];
            if (!source) {
                throw new Error(url + ' is missing!');
            }
            var exports = {};
            var module = {
                exports: exports
            };
            new Function('exports, module, require', source)(exports, module, require);
            require.cache[url] = module.exports;
            return module.exports;
        } else {
            return require.cache[url];
        }
    }

    require.cache = {};
    require.sourceCache = {};
    ">>.
