-module(commonjs_packager).

%% API
-export([bundle_single_js/2]).

%%%===================================================================
%%% API
%%%===================================================================
bundle_single_js(Js_entry_file, Watch_mode) ->
    State = #{
        entry_name       => Js_entry_file,
        source_cache     => #{},
        dependency_graph => #{}
    },

    New_state = bundle(Js_entry_file, "", State),
    Bundled_content = write_bundled_file(Js_entry_file, New_state),
    case Watch_mode of
        true ->
            rebuild_entry_if_module_changed(New_state);
        _ -> do_nothing
    end,
    Bundled_content.

%%%===================================================================
%%% Internal functions
%%%===================================================================
bundle(Js_entry_file, Ext_name, State) ->
    case file:read_file(Js_entry_file ++ Ext_name) of
        {ok, Content} ->
            Require_regexp = "(require\\((['|\"])(.*?)\\g2\\);?)",
            Replaced = re:replace(remove_comments(Content), Require_regexp, ["require('", filename:join(filename:dirname(Js_entry_file), "\\g3"), "')"], [global]),
            Map = maps:get(source_cache, State),
            State1 = maps:put(source_cache, Map#{list_to_binary(Js_entry_file) => iolist_to_binary([Replaced, "\n//# sourceURL=", Js_entry_file])}, State),
            Dependency_map = maps:get(dependency_graph, State),
            case re:run(Replaced, Require_regexp, [global,{capture,[3],list}]) of
                {match, Matched} ->
                    Old_dependencies = maps:get(Js_entry_file, Dependency_map, []),
                    Current_dependencies = [Module || [Module] <- Matched],
                    case Current_dependencies =:= Old_dependencies of
                        true -> State1;
                        false ->
                            State2 = maps:put(dependency_graph, Dependency_map#{Js_entry_file => Current_dependencies}, State1),
                            lists:foldl(
                                fun(Required_js_path, State) ->
                                    case lists:suffix(".js", Required_js_path) of
                                        true ->
                                            bundle(Required_js_path, "", State);
                                        false ->
                                            bundle(Required_js_path, ".js", State)
                                    end
                                end
                                , State2, Current_dependencies)
                    end;
                nomatch ->
                    State1
            end;
        {error, enoent} ->
            error_log(Js_entry_file ++ " is missing"),
            State
    end.

get_bundled_content(Js_entry_file, State) ->
    iolist_to_binary(["(function(){\n",
        js_require_function(),
        "\nrequire.sourceCache = ",
        jsx:prettify(jsx:encode(maps:get(source_cache, State))),
        ";\n",
        maps:get(list_to_binary(Js_entry_file), maps:get(source_cache, State)),
        ";\n})();"]).

write_bundled_file(Js_entry_file, State) ->
    Bundled_content = get_bundled_content(Js_entry_file, State),
    io:format("~p bundled to:~p ~n", [Js_entry_file, Js_entry_file ++ "-bundled.js"]),
    file:write_file(Js_entry_file ++ "-bundled.js", Bundled_content),
    Bundled_content.

rebuild_entry_if_module_changed(State) ->
    receive
        {file_changed, File} ->
            io:format("file_changed ~p ~n", [File]),
            New_state = lists:foldl(
                fun(Module_name, State) ->
                    case string_contains(File, binary_to_list(filename:join(Module_name, ""))) of
                        true ->
                            Required_module = binary_to_list(Module_name),
                            case lists:suffix(".js", Required_module) of
                                true ->
                                    State1 = maps:put(entry_name, Required_module, State),
                                    bundle(Required_module, "", State1);
                                false ->
                                    State1 = maps:put(entry_name, Required_module ++ ".js", State),
                                    bundle(Required_module, ".js", State1)
                            end,
                            write_bundled_file(maps:get(entry_name), State1),
                            State1;
                        false ->
                            State
                    end
                end,
                State, maps:keys(maps:get(source_cache, State))),
            rebuild_entry_if_module_changed(New_state)
    end.

string_contains(Big, Small)->
    string:str(Big, Small) > 0.

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