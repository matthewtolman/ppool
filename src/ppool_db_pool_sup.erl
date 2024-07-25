-module(ppool_db_pool_sup).

-behaviour(supervisor).

-export([query/2, query/3, start_link/1, start_link/0]).
-export([init/1]).

start_link() ->
    start_link([]).

start_link(Args=#{}) ->
    start_link(maps:to_list(Args));
start_link(Args) when is_list(Args) ->
    SupName = proplists:get_value(sup_name, Args, {local, ppool_db_pool_sup}),
    SupName1 =
        case SupName of
            P when is_atom(P) ->
                {local, P};
            _ ->
                SupName
        end,
    supervisor:start_link(SupName1, ?MODULE, Args).

init(Args=#{}) ->
    init(maps:to_list(Args));
init(Args) ->
    ArgPools = proplists:get_value(pools, Args, []),
    EnvPools =
        try
            application:get_env(ppool, pools, [])
        catch
            _ ->
                []
        end,
    Pools = ArgPools ++ EnvPools,
    DefaultPool =
        proplists:get_value(default_pool,
                            Args,
                            application:get_env(ppool, default_pool, first_pool(Pools))),
    DefaultIdSuffix = case proplists:get_value(id_suffix, Args, application:get_env(ppool, id_suffix, <<"_id">>)) of
                          Suffix when is_binary(Suffix) -> Suffix;
                          List when is_list(List) -> list_to_binary(List);
                          Atom when is_atom(Atom) -> atom_to_binary(Atom)
                      end,
    Inflection = proplists:get_value(use_inflection, Args, application:get_env(ppool, use_inflection, true)),
    InflectionMethod = proplists:get_value(inflection_method, Args, application:get_env(ppool, inflection_method, {ppool_inflection, singular})),

    PoolSpecs = lists:map(fun(PoolSpec) -> make_pool(PoolSpec) end, Pools),
    DefaultsSpec =
        #{id => ppool_defaults,
          start => {ppool_defaults, start_link,
                    [[{default_pool, DefaultPool},
                      {default_id_suffix, DefaultIdSuffix},
                      {use_inflection, Inflection},
                      {inflection_method, InflectionMethod}]]},
          restart => permanent,
          type => worker},
    {ok, {{one_for_one, 10, 1000}, [DefaultsSpec | PoolSpecs]}}.

query(PoolName, Sql) ->
    poolboy:transaction(PoolName, fun(Worker) -> ppool_db_worker:equery(Worker, Sql, []) end).

query(PoolName, Stmt, Params) ->
    poolboy:transaction(PoolName, fun(Worker) -> ppool_db_worker:equery(Worker, Stmt, Params) end).

make_pool(#{name := Name,
            pool := SizeArgs,
            db := WorkerArgs}) ->
    make_pool({Name, SizeArgs, WorkerArgs});
make_pool(#{name := Name,
           db := WorkerArgs}) ->
    make_pool({Name, #{size => 10}, WorkerArgs});
make_pool({Name, SizeArgs, WorkerArgs}) ->
    SizeArgs1 = ppool_conf:standardize_conf(SizeArgs),
    Size = ppool_conf:get_conf_value(integer, size, SizeArgs1, 10),
    Overflow = ppool_conf:get_conf_value(integer, max_overflow, SizeArgs1, 10),
    Strategy = case ppool_conf:get_conf_value(atom, strategy, SizeArgs1, lifo) of
                   lifo -> lifo;
                   fifo -> fifo;
                   V -> throw({invalid_conf, [{key, strategy}, {value, V}, {expected, [lifo, fifo]}]})
               end,

    WorkerArgs1 =
        if is_map(WorkerArgs) ->
               maps:to_list(WorkerArgs);
           true ->
               WorkerArgs
        end,
    SizeOpts = [{size, Size}, {max_overflow, Overflow}, {strategy, Strategy}],
    PoolArgs = [{name, {local, Name}}, {worker_module, ppool_db_worker}] ++ SizeOpts,
    poolboy:child_spec(Name, PoolArgs, WorkerArgs1).

first_pool([#{name:=Name} | _]=_Pools) -> Name;
first_pool([{Name, _, _} | _]) -> Name;
first_pool(_) -> default_pool.

