%% ----------------------------------
%%
%% @author Matthew Tolman
%% @copyright 2024 Matthew Tolman
%% @doc Root application.
%% @end
%% @version 0.1.1
%% @end
%%
%% ----------------------------------

-module(ppool_app).

-behaviour(application).

-export([start/2, stop/1]).

%% ----------------------------------
%%
%% @doc Starts the postgress pool (ppool) app.
%% @end
%%
%% ----------------------------------
start(_StartType, _StartArgs) ->
    ppool_sup:start_link().

%% ----------------------------------
%%
%% @doc Stops the postgres pool (ppool) app.
%% @end
%%
%% ----------------------------------
stop(_State) ->
    ok.

