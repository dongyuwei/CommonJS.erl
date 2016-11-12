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

%%====================================================================
%% Internal functions
%%====================================================================
bundle(Js_entry_file, Ext_name) ->
    case file:read_file(Js_entry_file ++ Ext_name) of
        {ok, Content} ->
            Require_regexp = "(require\\((['|\"])(.*?)\\g2\\);?)",
            Replaced = re:replace(remove_comments(Content), Require_regexp, ["require('", filename:join(filename:dirname(Js_entry_file), "\\g3"), "')"], [global]),
            put(list_to_binary(Js_entry_file), iolist_to_binary(Replaced)), 
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
