%%%-------------------------------------------------------------------
%% @doc ppool public API
%% @end
%%%-------------------------------------------------------------------

-module(ppool_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ppool_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
