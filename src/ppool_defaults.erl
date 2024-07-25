-module(ppool_defaults).

-export([start_link/1, start_link/0]).
-export([default_pool/0, default_id_suffix/0, use_inflection/0, inflection_method/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, code_change/3]).

-behavior(gen_server).

-record(state, {pool, id_suffix, use_inflection, inflection_method}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% Client Methods

default_pool() ->
    gen_server:call(?MODULE, default_pool).

default_id_suffix() ->
    gen_server:call(?MODULE, default_id_suffix).

use_inflection() ->
    gen_server:call(?MODULE, use_inflection).

inflection_method() ->
    gen_server:call(?MODULE, inflection_method).

%% Gen Server Init

init(Args) ->
    Args1 =
        if is_map(Args) ->
               maps:to_list(Args);
           true ->
               Args
        end,
    Pool = proplists:get_value(default_pool, Args1, default_pool),
    IdSuffix = proplists:get_value(default_id_suffix, Args1, <<"_id">>),
    UseInflection = proplists:get_value(use_inflection, Args1, true),
    InflectionMethod =
        proplists:get_value(inflection_method, Args1, {ppool_inflection, singular}),
    {ok,
     #state{pool = Pool,
            id_suffix = IdSuffix,
            use_inflection = UseInflection,
            inflection_method = InflectionMethod}}.

handle_call(use_inflection, _From, #state{use_inflection = Res} = S) ->
    {reply, Res, S};
handle_call(inflection_method, _From, #state{inflection_method = Res} = S) ->
    {reply, Res, S};
handle_call(default_id_suffix, _From, #state{id_suffix = IdSuffix} = S) ->
    {reply, IdSuffix, S};
handle_call(default_pool, _From, #state{pool = Pool} = S) ->
    {reply, Pool, S};
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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Implementation
