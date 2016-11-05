-module (packager).
-export ([bundle_js_in_dir/1, bundle/1]).

bundle_js_in_dir(Input_dir) ->
    Js_files = filelib:wildcard(filename:join(Input_dir, filename:join("**", "*.js"))),
    Js_files2 = lists:filter(
        fun(Js_file) -> 
            lists:prefix("_", filename:basename(Js_file)) =:= false 
        end, 
    Js_files),

    lists:foreach(
        fun(Js_file) ->
            spawn(?MODULE, bundle, [Js_file])
        end, 
    Js_files2) .


bundle(Js_entry_file) ->
    bundle2(Js_entry_file, Js_entry_file),
    io:format("~p ~n", [get()]),
    [_, _, _, {compile, [_, _, {source, Source}]}, _, _] = ?MODULE:module_info(),
    {ok, Require_fun} = file:read_file(filename:join(filename:dirname(Source), "require.js")),
    io:format("~p ~n", [Require_fun]).
    % "(function(){\n" + Require_fun + \nrequire.sourceCache = {};\n" + source "\n})();",
    % file:write_file(Js_entry_file + "-packed.js", Content).

bundle2(Js_entry_file, Required_module_name) ->
    io:format("processing ~p ~n", [Required_module_name]),
    {ok, Content} = file:read_file(Js_entry_file),
    put(Required_module_name, Content), 
    Require_regexp = "(require\\((['|\"])(.*?)\\g2\\);?)",
    case re:run(Content, Require_regexp, [global,{capture,[3],list}]) of
    {match, Matched} ->
        lists:foreach(
            fun(Item) -> 
                [Required_module] = Item,
                Required_js_path = filename:join(filename:dirname(Js_entry_file), Required_module),
                bundle2(Required_js_path, Required_module)
            end
        , Matched);
    nomatch ->
        nomatch
    end .

