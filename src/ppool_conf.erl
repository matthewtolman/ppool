%% ----------------------------------
%%
%% @author Matthew Tolman
%% @copyright 2024 Matthew Tolman
%% @doc Gets configurations for ppool. Private, should not be used externally.
%% @end
%% @version 0.1.1
%% @end
%%
%% @hidden
%%
%% ----------------------------------
-module(ppool_conf).

-export([get_conf_value/3, get_conf_value/4, standardize_conf/1, is_defined/2,
        get_raw_value/2, get_raw_value/3]).

get_conf_value(Type, Key, Args) ->
    Value = proplists:get_value(Key, Args),
    case resolve_conf_value(Type, Value) of
        undefined -> throw({missing_required_conf, Key, {type, Type}});
        {value, V} -> V
    end.

get_conf_value(Type, Key, Args, Default) ->
    Value = proplists:get_value(Key, Args),
    case resolve_conf_value(Type, Value) of
        undefined -> Default;
        {value, V} -> V
    end.

resolve_conf_value(Type, {env, EnvKey}) ->
    case os:getenv(EnvKey) of
        false -> undefined;
        V -> case Type of
                 string -> {value, V};
                 integer -> {value, list_to_integer(V)};
                 float -> {value, list_to_float(V)};
                 atom -> {value, list_to_atom(V)};
                 binary -> {value, list_to_binary(V)};
                 boolean -> {value, string:to_lower(V) =:= "true"};
                 raw -> {value, V};
                 _ -> throw({env_get_error,
                             [{reason, cannot_resolve_type},
                              {type, Type},
                              {env_key, EnvKey}]})
             end
    end;
resolve_conf_value(_Type, {fn, Mod, Fun}) ->
    case Mod:Fun() of
        {value, V} -> {value, V};
        undefined -> undefined
    end;
resolve_conf_value(_Type, {chain, []}) ->
    undefined;
resolve_conf_value(Type, {chain, [Resolver | Rest]}) ->
    case resolve_conf_value(Type, Resolver) of
        undefined -> resolve_conf_value(Type, {chain, Rest});
        {value, V} -> {value, V}
    end;
resolve_conf_value(_Type, undefined) ->
    undefined;
resolve_conf_value(_Type, {value, V}) ->
    {value, V};
resolve_conf_value(_Type, V) ->
    {value, V}.

is_defined(Key, Args) ->
    proplists:is_defined(Key, Args).

get_raw_value(Key, Args) ->
    proplists:get_value(Key, Args).

get_raw_value(Key, Args, Default) ->
    proplists:get_value(Key, Args, Default).

standardize_conf(Conf=#{}) ->
    maps:to_list(Conf);
standardize_conf(Conf) -> Conf.
