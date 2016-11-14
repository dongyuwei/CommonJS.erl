-module(commonjs).

%% API exports
-export ([bundle_single_js/1, bundle_js_in_dir/2]).

%%====================================================================
%% API functions
%%====================================================================
bundle_js_in_dir(Input_dir, Watch_mode) ->
    case Watch_mode of
        true ->
            Pids = bundle_js_in_dir(Input_dir),
            watch(Input_dir, Pids);
        _ ->
            bundle_js_in_dir(Input_dir)
    end.
    
bundle_single_js(Js_entry_file) ->
    put("entry_name", Js_entry_file),
    case get("source_cache") =:= undefined of
        true ->
            put("source_cache", #{});
        false -> do_nothing
    end,
    bundle(Js_entry_file, ""),
    Bundled_content = iolist_to_binary(["(function(){\n", 
                                       js_require_function(), 
                                       "\nrequire.sourceCache = ", 
                                       jsx:prettify(jsx:encode(get("source_cache"))), 
                                       ";\n",
                                       maps:get(list_to_binary(Js_entry_file), get("source_cache")),
                                       ";\n})();"]),
    io:format("~p bundled to:~p ~n", [Js_entry_file, Js_entry_file ++ "-bundled.js"]),
    file:write_file(Js_entry_file ++ "-bundled.js", Bundled_content),
    rebuild_entry_if_module_changed().

%%====================================================================
%% Internal functions
%%====================================================================
rebuild_entry_if_module_changed() ->
    receive
        {file_changed, File} ->
            io:format("file_changed ~p ~n", [File]),
            lists:foreach(
                fun(Key) -> 
                    case string_contains(File, binary_to_list(filename:join(Key, ""))) of
                        true ->
                            bundle_single_js(get("entry_name"));
                        false -> do_nothing
                    end
                end, 
            maps:keys(get("source_cache")))
    end,
    rebuild_entry_if_module_changed().

string_contains(Big, Small)->
    string:str(Big, Small) > 0.

bundle_js_in_dir(Input_dir) ->
    Js_files = filelib:wildcard(filename:join(Input_dir, filename:join("**", "*.js"))),
    Files = lists:filter(
        fun(Js_file) ->
            lists:prefix("_", filename:basename(Js_file)) =:= false
        end, 
    Js_files),
    loop_and_return_pids(Files, []).

loop_and_return_pids([], Acc) ->
    Acc;
loop_and_return_pids([H|T], Acc) ->
    Pid = spawn(?MODULE, bundle_single_js, [H]),
    loop_and_return_pids(T, [Pid|Acc]).

watch(Dir, Pids) ->
    io:format("watching ~p ~n", [Dir]),
    fs:start_link(fs_watcher, filename:absname(Dir)),
    fs:subscribe(fs_watcher),
    on_file_changed(Pids).

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
            case re:run(Replaced, Require_regexp, [global,{capture,[3],list}]) of
                {match, Matched} ->
                    lists:foreach(
                        fun(Item) -> 
                            [Required_js_path] = Item,
                            case lists:suffix(".js", Required_js_path) of
                                true ->
                                    bundle(Required_js_path, "");
                                false -> 
                                    bundle(Required_js_path, ".js")
                            end
                        end
                    , Matched);
                nomatch -> nomatch
            end;
        {error, enoent} -> 
            throw(Js_entry_file ++ " is missing")
    end.

%% only remove single line comments right now.
remove_comments(Content) ->
    re:replace(Content, "//.*", <<"">>, [global]).

js_require_function() ->
    %% see https://github.com/marijnh/Eloquent-JavaScript/blob/master/10_modules.txt#L465
    <<"
    function require(url){
        if(!require.cache[url]){
            var source = require.sourceCache[url];
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
