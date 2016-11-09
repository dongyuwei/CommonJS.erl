-module(commonjs).

%% API exports
-export ([bundle_js_in_dir/1, bundle_single_js/1]).

%%====================================================================
%% API functions
%%====================================================================
bundle_js_in_dir(Input_dir) ->
    Js_files = filelib:wildcard(filename:join(Input_dir, filename:join("**", "*.js"))),
    lists:foreach(
        fun(Js_file) ->
            case lists:prefix("_", filename:basename(Js_file)) of
                false ->
                    spawn(?MODULE, bundle_single_js, [Js_file]);
                true -> do_nothing
            end
        end, 
    Js_files) .


bundle_single_js(Js_entry_file) ->
    bundle(Js_entry_file),
    [_, _, _, {compile, [_, _, {source, Source}]}, _, _] = ?MODULE:module_info(),
    {ok, Require_fun} = file:read_file(filename:join(filename:dirname(Source), "require.js")),
    Bundled_content = iolist_to_binary(["(function(){\n", 
                                       Require_fun, 
                                       "\nrequire.sourceCache = ", 
                                       jsx:encode(get()), 
                                       ";\n",
                                       get(list_to_binary(Js_entry_file)),
                                       ";\n})();"]),
    io:format("~p bundled to:~p ~n", [Js_entry_file, Js_entry_file ++ "-bundled.js"]),
    file:write_file(Js_entry_file ++ "-bundled.js", Bundled_content).

%%====================================================================
%% Internal functions
%%====================================================================
bundle(Js_entry_file) ->
    case file:read_file(Js_entry_file) of
        {ok, Content}        ->
            Require_regexp = "(require\\((['|\"])(.*?)\\g2\\);?)",
            Replaced = re:replace(remove_comments(Content), Require_regexp, ["require('", filename:join(filename:dirname(Js_entry_file), "\\g3"), "')"], [global]),
            put(list_to_binary(Js_entry_file), iolist_to_binary(Replaced)), 
            case re:run(Replaced, Require_regexp, [global,{capture,[3],list}]) of
                {match, Matched} ->
                    lists:foreach(
                        fun(Item) -> 
                            [Required_js_path] = Item,
                            bundle(Required_js_path)
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
