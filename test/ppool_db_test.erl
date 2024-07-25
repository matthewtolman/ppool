-module(ppool_db_test).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/1, start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, code_change/3]).

-behavior(gen_server).

-record(state, {table, key}).

start_link() ->
        start_link([]).

start_link(Args) ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% Client Methods

table() ->
        gen_server:call(?MODULE, table).

key_col() ->
        gen_server:call(?MODULE, key).

reset_db() ->
        gen_server:call(?MODULE, reset_db).

%% Gen Server Init

init(_) ->
        Id = integer_to_binary(rand:uniform(999999999999999999999999999999999999999999999999)),
        TableId = <<"test_", Id/binary, "_links">>,
        {ok, _PoolPid} =
                ppool_db_pool_sup:start_link(#{sup_name => conn_test_pool_sup,
                                               pools =>
                                                       [#{name => conn_test_pool,
                                                          pool => #{size => 10, max_overflow => 20},
                                                          db =>
                                                                  #{hostname =>
                                                                            {chain,
                                                                             [{env, "DB_HOST"},
                                                                              "pop-os"]},
                                                                    database =>
                                                                            {chain,
                                                                             [{env, "DB_DATABASE"},
                                                                              "nindex"]},
                                                                    username =>
                                                                            {chain,
                                                                             [{env, "DB_USER"},
                                                                              "nindex_user"]},
                                                                    password =>
                                                                            {chain,
                                                                             [{env, "DB_PWD"},
                                                                              "nindex_pw"]},
                                                                    port =>
                                                                            {chain,
                                                                             [{env, "DB_PORT"},
                                                                              5433]}}}]}),
        ensure_table_setup(TableId),
        STable = ppool_inflection:singular(TableId),
        {ok, #state{table = TableId, key = <<STable/binary, "_id">>}}.

handle_call(key, _From, State = #state{key = Key}) ->
        {reply, Key, State};
handle_call(table, _From, State = #state{table = Table}) ->
        {reply, Table, State};
handle_call(table, _From, State = #state{table = Table}) ->
        ensure_table_setup(Table),
        {reply, ok, State};
handle_call(_Msg, _From, State = #state{}) ->
        {reply, {error, bad_msg}, State};
handle_call(_Msg, _From, State) ->
        {stop, bad_state, {error, bad_msg}, State}.

handle_cast(_Msg, State = #state{}) ->
        {noreply, State}.

handle_continue(_Msg, State) ->
        {noreply, State}.

handle_info(_Msg, State) ->
        {noreply, State}.

terminate(_Reason, #state{table = Table}) ->
        {ok, _} = ppool_db:query(<<"DROP TABLE IF EXISTS ", Table/binary>>),
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

ensure_table_setup(Table) ->
        {ok, _} = ppool_db:query(["DROP TABLE IF EXISTS ", Table]),
        {ok, _} =
                ppool_db:query(["CREATE TABLE ",
                                Table,
                                "(",
                                ppool_inflection:singular(Table),
                                "_id SERIAL NOT NULL PRIMARY KEY, descriptor text, topic text, "
                                "url text );"]),
        {ok, _} =
                ppool_db:query(["INSERT INTO ",
                                Table,
                                " (descriptor, topic, url) VALUES ('Google Search', 'search', "
                                "'https://google.com'), ('Example Site', 'example', 'https://example."
                                "com');"]),
        timer:sleep(100).

ensure_started() ->
        case start_link() of
                {ok, Pid} ->
                        Pid;
                {error, {already_started, Pid}} ->
                        reset_db(),
                        Pid
        end.

shutdown(_) ->
        sys:terminate(?MODULE, normal),
        wait_for_shutdown().

wait_for_shutdown() ->
        case whereis(?MODULE) of
                undefined ->
                        ok;
                _ ->
                        timer:sleep(10),
                        wait_for_shutdown()
        end.

conn_test_() ->
        [{setup,
          fun ensure_started/0,
          fun shutdown/1,
          fun(_) ->
             Table = table(),
             conn_test_pool = ppool_defaults:default_pool(),
             <<"_id">> = ppool_defaults:default_id_suffix(),
             {ok, Res1} = ppool_db:query(["SELECT * FROM ", Table]),
             {Res2, _} = ppool_db:query("SELECT * FROM non_existant_table"),
             [?_assert(is_list(Res1)), ?_assertEqual(error, Res2)]
          end}].

initial_data(Key) ->
        [#{"descriptor" => <<"Google Search">>,
           Key => 1,
           "topic" => <<"search">>,
           "url" => <<"https://google.com">>},
         #{"descriptor" => <<"Example Site">>,
           Key => 2,
           "topic" => <<"example">>,
           "url" => <<"https://example.com">>}].

save_test_() ->
        [{foreach,
          fun ensure_started/0,
          fun shutdown/1,
          [fun(_) ->
              Table = table(),
              Key = binary_to_list(key_col()),
              conn_test_pool = ppool_defaults:default_pool(),
              true = ppool_defaults:use_inflection(),
              Expected = initial_data(Key),
              <<"_id">> = ppool_defaults:default_id_suffix(),
              Res1 = ppool_db:get_all(Table),
              [?_assertEqual({ok, Expected}, Res1)]
           end,
           fun(_) ->
              Table = table(),
              Key = binary_to_list(key_col()),
              NewRecord =
                      #{descriptor => <<"Author Blog">>,
                        "topic" => "blog",
                        <<"url">> => "https://matthewtolman.com"},
              ppool_db:save(Table, NewRecord),
              NewRow = #{"descriptor" => <<"Author Blog">>,
                         "topic" => <<"blog">>,
                         "url" => <<"https://matthewtolman.com">>,
                         Key => 3},

              Expected = initial_data(Key),
              Expected2 = Expected ++ [NewRow],
              Res2 = ppool_db:get_all(Table, [{order_by, Key}]),
              Res3 = ppool_db:get_by_id(Table, 3),
              [?_assertEqual({ok, Expected2}, Res2), ?_assertEqual({ok, NewRow}, Res3)]
           end,
           fun(_) ->
              Table = table(),
              Key = binary_to_list(key_col()),
              UpdateRecord =
                      #{Key => 2,
                        descriptor => <<"Update Examples">>,
                        "topic" => "examples",
                        <<"url">> => "https://www.example.com"},
              ppool_db:save(Table, UpdateRecord),
              Expected =
                      #{Key => 2,
                        "descriptor" => <<"Update Examples">>,
                        "topic" => <<"examples">>,
                        "url" => <<"https://www.example.com">>},
              Res = ppool_db:get_by_id(Table, 2),
              [?_assertEqual({ok, Expected}, Res)]
           end]}].

delete_test_() ->
        [{setup,
          fun ensure_started/0,
          fun shutdown/1,
          fun(_) ->
             Table = table(),
             Key = binary_to_list(key_col()),
             Data = initial_data(Key),
             {_, Res1} = ppool_db:get_by_id(Table, 1),
             {_, Res2} = ppool_db:get_all(Table),
             ppool_db:delete(Table, 1),
             {_, Res3} = ppool_db:get_by_id(Table, 1),
             {_, Res4} = ppool_db:get_all(Table),
             [?_assertEqual(lists:nth(1, Data), Res1),
              ?_assertEqual(2, length(Res2)),
              ?_assertEqual(not_found, Res3),
              ?_assertEqual(1, length(Res4))]
          end}].
