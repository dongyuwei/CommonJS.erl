-module(commonjs_tests).
-include_lib("eunit/include/eunit.hrl").

bundle_single_js_test() ->
  os:cmd("rm /tmp/commonjs_build/entry_bundled.js"),

  Bundled_content = commonjs:bundle_single_js("./test/commonjs/entry.js", "/tmp/commonjs_build/entry_bundled.js"),
  {ok, Bundled_file_content} = file:read_file("/tmp/commonjs_build/entry_bundled.js"),
  ?assertEqual(Bundled_content, Bundled_file_content),

  {ok, Content} = file:read_file("test/commonjs/entry.js.txt"),
  ?assertEqual(Content, list_to_binary(os:cmd("node /tmp/commonjs_build/entry_bundled.js"))).


bundle_js_in_dir_test() ->
  os:cmd("rm -rf /tmp/commonjs_build"),
  commonjs:bundle_js_in_dir("./test", "/tmp/commonjs_build", false),
  Bundled_count = string:strip(string:strip(os:cmd("ls /tmp/commonjs_build/commonjs/*.js | wc -l")), both, $\n),
  ?assertEqual(Bundled_count, "2").

rebuild_changed_js_test() ->
    os:cmd("rm /tmp/commonjs_build/commonjs/entry2.js"),
    os:cmd("git checkout test/commonjs/entry2.js"),
    commonjs:bundle_js_in_dir("./test", "/tmp/commonjs_build", true),
    ?assertNotEqual(os:cmd("node /tmp/commonjs_build/commonjs/entry2.js"), "999\n"),
    os:cmd("echo 'console.log(999)' > ./test/commonjs/entry2.js"),

    timer:sleep(500), % maybe need more timeout to wait until the rebuild finished

    ?assertEqual(os:cmd("node /tmp/commonjs_build/commonjs/entry2.js"), "999\n"),
    os:cmd("git checkout test/commonjs/entry2.js").