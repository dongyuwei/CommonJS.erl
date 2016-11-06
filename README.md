CommonJS.erl
=====
A CommonJS Packager in Erlang.

Created by `rebar3 new lib commonjs`

# Project Goal: speed, speed, and speed!
1. Bundle Common JS modules concurrently using Erlang actors. It is expected to work like [browserify](https://github.com/substack/node-browserify)
2. Watch and incrementally rebuild Common JS modules concurrently using Erlang actors. It is expected to work like [watchify](https://github.com/substack/watchify)
3. It's expected to be a tiny and simple CommonJS Packager. I do't plan to implement all the features like browserify.

## Status
The core features of Common Js packager/loader is done(It can bundle the `tests/commonjs/entry.js` now). Still work in progress. 


Build
-----

    $ rebar3 compile # or `proxychains4  rebar3 compile`


Test in erl shell:
-----
    $ rm ./tests/**/*-bundled.js
    $ rebar3 shell
    $ r3:do(compile).
    $ commonjs:bundle_js_in_dir('./tests/commonjs/').
