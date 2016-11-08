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
    bundle(Js_entry_file, Js_entry_file),
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
bundle(Js_entry_file, Required_module_name) ->
    case file:read_file_info(Js_entry_file) of
        {ok, _}         ->
            {ok, Content} = file:read_file(Js_entry_file),
            put(list_to_binary(Required_module_name), Content), 
            Require_regexp = "(require\\((['|\"])(.*?)\\g2\\);?)",
            case re:run(Content, Require_regexp, [global,{capture,[3],list}]) of
            {match, Matched} ->
                lists:foreach(
                    fun(Item) -> 
                        [Required_module] = Item,
                        Required_js_path = filename:join(filename:dirname(Js_entry_file), Required_module),
                        bundle(Required_js_path, Required_module)
                    end
                , Matched);
            nomatch ->
                nomatch
            end;
        {error, enoent} -> 
            io:format("~s is missing~n", [Js_entry_file]);
        {error, Reason} -> 
            io:format("~s is ~s~n", [Js_entry_file, Reason])
    end.
