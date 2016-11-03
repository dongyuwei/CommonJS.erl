-module (packager).
-export ([bundle_single_js/1, bundle/2]).

bundle(Input_dir, Output_dir) ->
    Js_files = filelib:wildcard(filename:join(Input_dir, filename:join("**", "*.js"))),
    lists:foreach(fun(Js_file) -> bundle_single_js(Js_file) end, Js_files).

bundle_single_js(Js_entry_file) ->
    bundle_single_js2(Js_entry_file, Js_entry_file).


bundle_single_js2(Js_entry_file, Required_name) ->
    io:format("load ~p ~n", [Required_name]),
    {ok, Content} = file:read_file(Js_entry_file),
    put(Required_name, Content), 
    Require_regexp = "(require\\((['|\"])(.*?)\\g2\\);?)",
    case re:run(Content, Require_regexp, [global,{capture,[3],list}]) of
    {match, Matched} ->
        lists:foreach(
            fun(Item) -> 
                [Required_module] = Item,
                Required_js_path = filename:join(filename:dirname(Js_entry_file), Required_module),
                bundle_single_js2(Required_js_path, Required_module)
            end
        , Matched);
    nomatch ->
        nomatch
    end,
    % https://www.erlang.org/course/advanced#dict
    get().
