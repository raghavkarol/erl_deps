erl_deps
=====

Escript that creates a dot file with *rebar2* project dependencies

Build
-----

    $ rebar3 compile
    $ rebar3 escriptize # produces an escript called erl_deps

Run
---

    $ erl_deps --fetch-deps --out-file ../github/riak_kv/riak_kv.dot --path-to-app ../github/riak_kv
