-module(erl_deps).

%% API exports
-export([main/1,
         main/2,
         default_opts/0]).

-type opt() :: get_deps
             | no_get_deps.

-type opts() :: [opt()].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type dot() :: binary().

-record(app, {name :: atom(),
              path :: string(),
              rebar3 :: boolean()}).

-record(dep, {name :: atom(),
              path :: string()}).

-record(app_deps, {app ::  #app{},
                   deps :: [#dep{}]}).

%%====================================================================
%% API functions
%%====================================================================
-spec main(string()) -> term().
main(App) ->
    main(App, default_opts()).

-spec main(string(), opts()) -> term().
main(App, Opts) ->
    ok = maybe_rebar_get_deps(get_opt(get_deps, Opts),
                              App),
    AllDeps = lists:map(fun(Dep) ->
                                #app_deps{
                                   app =
                                       #app{name = filename:basename(Dep),
                                            path = Dep,
                                            rebar3 = has_rebar3_feature_branch(Dep)},
                                   deps = [#dep{name = atom_to_list(DepName),
                                                path = filename:dirname(App) ++ "/dep/" ++ atom_to_list(DepName)}
                                           || {DepName, _, _} <- rebar_config_deps(Dep)]}
                        end,
                        [App| ls_deps_dir(App)]),
    DotData = to_dot_digraph(AllDeps),
    ok = file:write_file("/Users/raghav/github/erl_deps/" ++ dot_filename(App), DotData).


default_opts() ->
    [get_deps].
%%====================================================================
%% Internal functions
%%====================================================================
dot_filename(App) ->
    filename:basename(App) ++ ".dot".

indent() ->
    "  ".

to_dot_dep_line(A, B) ->
    [A, " -> ", B, ";" "\n"].


-spec to_dot_props(#app_deps{}) -> [{Key :: atom(), Value :: atom()}].
to_dot_props(#app_deps{} = AppDeps) ->
    default_props()
        ++ color_props(AppDeps)
        ++ style_props(AppDeps)
        ++ [].

style_props(#app_deps{app = #app{rebar3 = true}}) ->
    [];
style_props(#app_deps{app = #app{rebar3 = false}}) ->
    [{style, dashed}].

color_props(#app_deps{deps = []}) ->            % Leaf node
    [{color, black}];

color_props(#app_deps{}) ->
    [{color, red}].

default_props() ->
    [{fontname, courier}].


-spec to_dot_digraph([#app_deps{}]) -> dot().
to_dot_digraph(AllDeps) ->
    iolist_to_binary(
      [
       "digraph G {", "\n",
       [[indent(), to_dot_dep_line(AppName, DepName)]
        || #app_deps{app = #app{name = AppName},
                     deps = Deps} <- AllDeps,
           #dep{name = DepName} <- Deps],
       [
        [indent(),
         Name,
         to_iodata(to_dot_props(AppDeps)),
         "\n"]
        || #app_deps{app = #app{name = Name}} = AppDeps <- AllDeps],
       "}"
     ]).

rebar_config_deps(App) ->
    RebarConfig = rebar_config(App),
    proplists:get_value(deps, RebarConfig, []).

rebar_config(App) ->
    RebarConfig = App ++ "/rebar.config",
    case file:consult(RebarConfig) of
        {ok, Terms} ->
            Terms;
        {error, _} = E->
            io:format(user, "~n>>> ~s:~p -- ~p~n", [?MODULE, ?LINE, E]),
            []
    end.

ls_deps_dir(App) ->
    Cmd = cmd_str("ls ~s/deps", [App]),
    [App ++ "/deps/" ++ Dir || Dir <- run_cmd(Cmd)].

maybe_rebar_get_deps(true, AppPath) ->
    Cmd = cmd_str("(cd ~s ; rebar get-deps)", [AppPath]),
    run_cmd(Cmd),
    ok;

maybe_rebar_get_deps(_, _AppPath) ->
    ok.

run_cmd(Cmd) ->
    Result = os:cmd(Cmd),
    string:tokens(Result, "\n").

cmd_str(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

get_opt(Opt, Opts) ->
    proplists:get_value(Opt, Opts, undefined).


-spec has_rebar3_feature_branch(App :: string()) -> boolean().
has_rebar3_feature_branch(App) ->
    CmdStr = cmd_str("(cd ~s && git branch -a | grep feature/riak-2903/rebar3)",
                     [App]),
    case run_cmd(CmdStr) of
        [] -> false;
        _ ->
            true
    end.


-spec to_iodata([{Key :: atom(), Value :: atom()}]) -> iodata().
to_iodata(Props) ->
    [
     "[",
     string:join([io_lib:format("~s = \"~s\"", [K, V]) || {K, V} <- Props], ", "),
     "]"
    ].



-ifdef(TEST).

to_iodata_test() ->
    R = to_iodata([{fontname, courier}, {color, green}, {style, dotted}]),
    "[fontname = \"courier\", color = \"green\", style = \"dotted\"]" = lists:flatten(R),
    ok.

-endif.
