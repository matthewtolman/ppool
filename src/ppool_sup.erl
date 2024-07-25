%%%-------------------------------------------------------------------
%% @doc ppool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ppool_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 5,
                 period => 3600},
    ChildSpecs = [#{id => ppool_db_pool_sup,
                   start => {ppool_db_pool_sup, start_link, []},
                   type => supervisor,
                   modules => [ppool_db_pool_sup]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
