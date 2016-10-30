-module (packager).
-export ([bundle/2]).

bundle(Input_dir, Output_dir) ->
    Js_files = filelib:wildcard(filename:join(Input_dir, filename:join("**", "*.js"))),
    lists:foreach(fun(Js_File) -> bundle_single_js(Js_File) end, Js_files).

bundle_single_js(Js_File) ->
    {ok, Content} = file:read_file(Js_File),
    io:format("~p ~n", [Content]).