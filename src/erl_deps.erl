-module(erl_deps).

%% API exports
-export([main/1,
         do/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type dot() :: binary().

-record(app, {name :: atom(),
              path :: string()}).

-record(dep, {name :: atom(),
              path :: string()}).

-record(app_deps, {app ::  #app{},
                   deps :: [#dep{}]}).


-record(args, {fetch_deps = false,
               out_file = undefined,
               path_to_app = undefined}).

%%====================================================================
%% API functions
%%====================================================================
-spec parse_cmd_line(CmdLineArgs :: []) -> #args{} | no_return.
parse_cmd_line(CmdLineArgs) ->
    case getopt:parse(opt_spec_list(), CmdLineArgs) of
        {ok, {ParsedArgs, _Extra}} ->
            case Args = to_args(ParsedArgs) of
                #args{out_file = undefined} ->
                    throw(io_lib:format("Error: missing out-file ~n~n", []));
                #args{path_to_app = undefined} ->
                    throw(io_lib:format("Error: missing path-to-app ~n~n", []));
                Args ->
                    Args
            end;

        {error, {Reason, Data}} ->
            throw(io_lib:format("Error: ~s ~p~n~n", [Reason, Data]))
    end.

main(CmdLineArgs) ->
    try
        do(parse_cmd_line(CmdLineArgs))
    catch
        throw:Error ->
            io:format("Error: ~s ~n", [Error]),
            getopt:usage(opt_spec_list(), escript:script_name())
    end.

path_to_app_dep(AppPath, Path) ->
    #app_deps{
       app =
           #app{name = filename:basename(Path),
                path = Path},
       deps = [#dep{name = atom_to_list(DepName),
                    path = filename:dirname(AppPath) ++ "/dep/" ++ atom_to_list(DepName)}
               || {DepName, _, _} <- rebar_config_deps(Path)]}.

do(#args{fetch_deps = FetchDeps,
            out_file = OutFile,
            path_to_app = AppPath}) ->

    ok = maybe_rebar_get_deps(FetchDeps, AppPath),

    AllDeps = lists:map(fun(Path) -> path_to_app_dep(AppPath, Path)
                        end, [AppPath| ls_deps_dir(AppPath)]),
    DotData = to_dot_digraph(AllDeps),
    ok = file:write_file(OutFile, DotData).

%%====================================================================
%% Internal functions
%%====================================================================
opt_spec_list() ->
    [{fetch_deps,    $f,        "fetch-deps",    {boolean, false}, "calls rebar3 deps to fetch deps first"},
     {out_file,      $o,        "out-file",      string,           "output dot file name"},
     {path_to_app,   $p,        "path-to-app",   string,           "path the application that should be analyzed"}
    ].

to_args(Args) ->
    #args{fetch_deps  = proplists:get_value(fetch_deps, Args, false),
          out_file    = proplists:get_value(out_file, Args),
          path_to_app = proplists:get_value(path_to_app, Args) }.

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

style_props(#app_deps{}) ->
    [].

color_props(#app_deps{deps = []}) ->            % Leaf node
    [{color, green}];

color_props(#app_deps{}) ->
    [{color, brown}].

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
            io:format(user, "~n>>> ~s:~p -- ~p~n", [?MODULE, ?LINE, App]),
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

getopt_test() ->
    ParsedArgs1 = getopt:parse(opt_spec_list(), getopt:tokenize("-f -o x.dot")),
    {ok, {[{fetch_deps, true},
           {out_file,"x.dot"}], []}} = ParsedArgs1,

    ParsedArgs2 = getopt:parse(opt_spec_list(), getopt:tokenize("-o x.dot")),
    {ok, {[{out_file,"x.dot"},
           {fetch_deps, false}], []}} = ParsedArgs2,

    ParsedArgs3 = getopt:parse(opt_spec_list(), getopt:tokenize("-o x.dot -f0")),
    {ok, {[{out_file,"x.dot"},
           {fetch_deps, false}], []}} = ParsedArgs3,

    ok.


-endif.
