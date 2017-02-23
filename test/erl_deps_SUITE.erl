-module(erl_deps_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [F || {F, A} <- module_info(exports),
          is_testcase({F, A})].

test_one(_Config) ->
    erl_deps:main("/Users/raghav/github/riak_kv", [no_get_deps]),
    ok.

%% Internal functions
is_testcase({F, 1}) ->
    match =:= re:run(atom_to_list(F), "^test_", [{capture, none}]);
is_testcase({_, _}) ->
    false.
