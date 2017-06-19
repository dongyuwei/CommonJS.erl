-module(commonjs_tests).
-include_lib("eunit/include/eunit.hrl").

bundle_single_js_test() ->
  os:cmd("rm ./test/**/*-bundled.js"),

  Bundled_content = commonjs:bundle_single_js("./test/commonjs/entry.js"),
  {ok, Bundled_file_content} = file:read_file("./test/commonjs/entry.js-bundled.js"),
  ?assertEqual(Bundled_content, Bundled_file_content),

  {ok, Content} = file:read_file("test/commonjs/entry.js.txt"),
  ?assertEqual(Content, list_to_binary(os:cmd("node ./test/commonjs/entry.js-bundled.js"))).


bundle_js_in_dir_test() ->
  os:cmd("rm ./test/**/*-bundled.js"),
  commonjs:bundle_js_in_dir("./test", false),
  Bundled_count = string:strip(string:strip(os:cmd("ls ./test/**/*-bundled.js | wc -l")), both, $\n),
  ?assertEqual(Bundled_count, "2").
