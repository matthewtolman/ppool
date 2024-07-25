-module(ppool_db).

-export([query/1, query/2, save/2, save/3, insert/2, update/3, update/2, delete/3,
         delete/2, insert/3, update/4, delete/4, query/3, get_all/1, get_all/2, get_by_id/2,
         get_by_id/3, get_by_id/4]).

get_id_col(Table) ->
    Table1 = binaryify(Table),
    Base = case ppool_defaults:use_inflection() of
        true ->
            {Mod, Fun} = ppool_defaults:inflection_method(),
            Mod:Fun(Table1);
        _ -> Table1
       end,
    Suffix = ppool_defaults:default_id_suffix(),
    <<Base/binary, Suffix/binary>>.

query(Sql) ->
    query(ppool_defaults:default_pool(), Sql, []).

query(Pool, Sql) when is_atom(Pool) ->
    query(Pool, Sql, []);
query(Sql, Params) ->
    query(ppool_defaults:default_pool(), Sql, Params).

query(Pool, Sql, Params) ->
    case ppool_db_pool_sup:query(Pool, Sql, Params) of
        {ok, Cols, Rows} when is_list(Cols), is_list(Rows) ->
            {ok, to_map(map_names(Cols), Rows, [])};
        Res ->
            Res
    end.

get_all(Table) ->
    get_all(Table, []).

get_all(Table, Opts = #{}) ->
    get_all(Table, maps:to_list(Opts));
get_all(Table, Opts) ->
    get_all(ppool_defaults:default_pool(), Table, Opts).

get_all(Pool, Table, Opts) ->
    Limit = proplists:get_value(limit, Opts, 1000),
    Offset = proplists:get_value(offset, Opts, 0),
    OrderBy =
        case binaryify(proplists:get_value(order_by, Opts, <<>>)) of
            <<>> ->
                <<>>;
            O ->
                escape_sql_term(O)
        end,
    OrderDir =
        binaryify(string:to_upper(stringify(proplists:get_value(order_dir, Opts, <<"asc">>)))),
    OrderDir1 =
        case OrderDir of
            <<"ASC">> ->
                OrderDir;
            <<"DESC">> ->
                OrderDir;
            <<"DES">> ->
                <<"DESC">>;
            _ ->
                <<"ASC">>
        end,
    Table1 = escape_sql_term(Table),
    Sql = case OrderBy of
              <<>> ->
                  <<"SELECT * FROM ", Table1/binary, " LIMIT $1 OFFSET $2">>;
              _ ->
                  <<"SELECT * FROM ",
                    Table1/binary,
                    " ORDER BY ",
                    OrderBy/binary,
                    " ",
                    OrderDir1/binary,
                    " LIMIT $1 OFFSET $2">>
          end,
    Params = [Limit, Offset],
    query(Pool, Sql, Params).

get_by_id(Table, ID) ->
    Table1 = binaryify(Table),
    get_by_id(Table1, get_id_col(Table1), ID).

get_by_id(Table, KeyCol, ID) ->
    get_by_id(ppool_defaults:default_pool(), Table, KeyCol, ID).

get_by_id(Pool, Table, KeyCol, ID) ->
    Table1 = escape_sql_term(binaryify(Table)),
    Key = escape_sql_term(binaryify(KeyCol)),
    Sql = <<"SELECT * FROM ", Table1/binary, " WHERE ", Key/binary, " = $1">>,
    Params = [ID],
    case query(Pool, Sql, Params) of
        {ok, [V | _]} ->
            {ok, V};
        {ok, []} ->
            {error, not_found};
        Res ->
            Res
    end.

delete(Table, KeyVal) ->
    Table1 = binaryify(Table),
    delete(Table1, get_id_col(Table1), KeyVal).

delete(Table, KeyCol, KeyVal) ->
    delete(ppool_defaults:default_pool(), Table, KeyCol, KeyVal).

delete(Pool, Table, KeyCol, KeyVal) ->
    Table1 = escape_sql_term(Table),
    Key = escape_sql_term(binaryify(KeyCol)),
    Sql = <<"DELETE FROM ", Table1/binary, " WHERE ", Key/binary, " = $1">>,
    Params = [KeyVal],
    query(Pool, Sql, Params).

insert(Table, Item) ->
    insert(ppool_defaults:default_pool(), Table, Item).

insert(Pool, Table, Item) ->
    Table1 = binaryify(Table),
    ItemStandardKeys = keys_to_binary(Item),
    insert_item(Pool, Table1, ItemStandardKeys).

update(Table, Item) ->
    Table1 = binaryify(Table),
    update(Table1, get_id_col(Table1), Item).

update(Table, KeyCol, Item) ->
    update(ppool_defaults:default_pool(), Table, KeyCol, Item).

update(Pool, Table, KeyCol, Item) ->
    Key = escape_sql_term(binaryify(KeyCol)),
    Table1 = binaryify(Table),
    ItemStandardKeys = keys_to_binary(Item),
    case maps:is_key(Key, ItemStandardKeys) of
        true ->
            save(Pool, Table1, Key, ItemStandardKeys);
        _ ->
            {error, missing_key_field}
    end.

save(Table, Item = #{}) ->
    Table1 = binaryify(Table),
    save(Table1, get_id_col(Table1), Item).

save(Table, Key, Item = #{}) ->
    save(ppool_defaults:default_pool(), Table, Key, Item).

save(Pool, Table, Key, Item = #{}) ->
    Table1 = binaryify(Table),
    Key1 = binaryify(Key),
    ItemStandardKeys = keys_to_binary(Item),
    KeyVal = maps:get(Key1, ItemStandardKeys, undefined),
    ItemNoKey = maps:remove(Key1, ItemStandardKeys),
    case KeyVal of
        undefined ->
            insert_item(Pool, Table1, ItemNoKey);
        0 ->
            insert_item(Pool, Table1, ItemNoKey);
        _ ->
            update_item(Pool, Table1, Key1, KeyVal, ItemNoKey)
    end.

escape_sql_terms(Terms) ->
    lists:map(fun(T) -> escape_sql_term(T) end, Terms).

escape_sql_term(Term) ->
    TermNoQuote = join(binary:split(binaryify(Term), <<"\"">>)),
    Quote = <<"\"">>,
    <<Quote/binary, TermNoQuote/binary, Quote/binary>>.

insert_item(Pool, Table, Item) ->
    Cols = join(<<",">>, escape_sql_terms(maps:keys(Item))),
    Table1 = escape_sql_term(Table),
    ValsPlaceholders =
        join(<<",">>,
             lists:map(fun(N) ->
                          NB = integer_to_binary(N),
                          <<"$", NB/binary>>
                       end,
                       lists:seq(1, maps:size(Item)))),
    Sql = <<"INSERT INTO ",
            Table1/binary,
            " (",
            Cols/binary,
            ") VALUES (",
            ValsPlaceholders/binary,
            ")">>,
    Params = maps:values(Item),
    query(Pool, Sql, Params).

update_item(Pool, Table, KeyCol, KeyVal, Item) ->
    Zip = lists:zip(
              maps:keys(Item), lists:seq(2, maps:size(Item) + 1)),
    Sets =
        join(<<", ">>,
             lists:map(fun({K, I}) ->
                          KE = escape_sql_term(K),
                          IB = integer_to_binary(I),
                          J = <<" = $">>,
                          <<KE/binary, J/binary, IB/binary>>
                       end,
                       Zip)),
    Table1 = escape_sql_term(Table),
    Key = escape_sql_term(KeyCol),
    Sql = <<"UPDATE ", Table1/binary, " SET ", Sets/binary, " WHERE ", Key/binary, " = $1">>,
    Params = [KeyVal | maps:values(Item)],
    query(Pool, Sql, Params).

map_names(Cols) ->
    Cols1 = lists:map(fun(Col) -> tuple_to_list(Col) end, Cols),
    lists:map(fun([column, Name | _]) -> binary_to_list(Name) end, Cols1).

to_map(_Cols, [], L) ->
    lists:reverse(L);
to_map(Cols, [Row | Rows], L) ->
    to_map(Cols, Rows, [row_to_map(Cols, tuple_to_list(Row)) | L]).

row_to_map(Cols, Row) ->
    row_to_map(Cols, Row, #{}).

row_to_map(_, [], M) ->
    M;
row_to_map([], _, M) ->
    M;
row_to_map([Name | Cols], [Val | Vals], M) ->
    row_to_map(Cols, Vals, maps:put(Name, Val, M)).

keys_to_binary(Map) ->
    List = maps:to_list(Map),
    maps:from_list(
        lists:map(fun({Key, Value}) -> {binaryify(Key), Value} end, List)).

binaryify(V = [C | _]) when is_integer(C) ->
    list_to_binary(V);
binaryify([]) ->
    <<"">>;
binaryify(V) when is_atom(V) ->
    atom_to_binary(V);
binaryify(V) when is_integer(V) ->
    integer_to_binary(V);
binaryify(V) when is_float(V) ->
    float_to_binary(V);
binaryify(V) ->
    V.

stringify(V) when is_binary(V) ->
    binary_to_list(V);
stringify(V) when is_atom(V) ->
    atom_to_list(V);
stringify(V) when is_integer(V) ->
    integer_to_list(V);
stringify(V) when is_float(V) ->
    float_to_list(V);
stringify(V) ->
    V.

join(BinaryList) ->
    join(<<>>, BinaryList).

join(Sep, BinaryList) ->
    join(Sep, BinaryList, <<>>).

join(Sep, _BinaryList = [B1 | Rest], <<>>) ->
    join(Sep, Rest, <<B1/binary>>);
join(Sep, [B1 | Rest], Res) ->
    join(Sep, Rest, <<Res/binary, Sep/binary, B1/binary>>);
join(_Sep, [], Res) ->
    Res.
