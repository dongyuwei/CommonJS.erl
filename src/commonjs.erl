-module(commonjs).

%% API exports
-export ([bundle_single_js/1, bundle_js_in_dir/2, watch/1]).

%%====================================================================
%% API functions
%%====================================================================
bundle_js_in_dir(Input_dir, Watch_mode) ->
    case Watch_mode of
        true ->
            bundle_js_in_dir(Input_dir),
            watch(Input_dir);
        _ ->
            bundle_js_in_dir(Input_dir)
    end.
    

bundle_single_js(Js_entry_file) ->
    bundle(Js_entry_file, ""),
    Bundled_content = iolist_to_binary(["(function(){\n", 
                                       js_require_function(), 
                                       "\nrequire.sourceCache = ", 
                                       jsx:prettify(jsx:encode(get())), 
                                       ";\n",
                                       get(list_to_binary(Js_entry_file)),
                                       ";\n})();"]),
    io:format("~p bundled to:~p ~n", [Js_entry_file, Js_entry_file ++ "-bundled.js"]),
    file:write_file(Js_entry_file ++ "-bundled.js", Bundled_content).

watch(Dir) ->
    io:format("watching ~p ~n", [Dir]),
    fs:start_link(fs_watcher, filename:absname(Dir)),
    % fs:subscribe(fs_watcher),
    % on_file_changed().
    spawn(fun() -> fs:subscribe(fs_watcher), on_file_changed() end).

%%====================================================================
%% Internal functions
%%====================================================================
bundle_js_in_dir(Input_dir) ->
    Js_files = filelib:wildcard(filename:join(Input_dir, filename:join("**", "*.js"))),
    lists:foreach(
        fun(Js_file) ->
            case lists:prefix("_", filename:basename(Js_file)) of
                false ->
                    Pid = spawn(?MODULE, bundle_single_js, [Js_file]),
                    io:format("Pid:~p ~n", [Pid]);
                true -> do_nothing
            end
        end, 
    Js_files) .

on_file_changed() ->
    receive
        {_Pid, {fs,file_event}, {File, [inodemetamod,modified]}} ->
            io:format("~p ~n", [File ++ " changed"]);
        _ -> ignore
    end,
    on_file_changed().

bundle(Js_entry_file, Ext_name) ->
    case file:read_file(Js_entry_file ++ Ext_name) of
        {ok, Content} ->
            Require_regexp = "(require\\((['|\"])(.*?)\\g2\\);?)",
            Replaced = re:replace(remove_comments(Content), Require_regexp, ["require('", filename:join(filename:dirname(Js_entry_file), "\\g3"), "')"], [global]),
            put(list_to_binary(Js_entry_file), iolist_to_binary([Replaced, "\n//# sourceURL=", Js_entry_file])), 
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
