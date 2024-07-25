-module(ppool_db_worker).

-behavior(gen_server).
-behavior(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([squery/2, equery/3]).

-record(state, {conn}).

squery(Worker, Sql) ->
    gen_server:call(Worker, {squery, Sql}).

equery(Worker, Stmt, Params) ->
    gen_server:call(Worker, {equery, Stmt, Params}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    Args1 = if is_map(Args) -> maps:to_list(Args);
               true -> Args
            end,
    process_flag(trap_exit, true),
    Hostname = proplists:get_value(hostname, Args1),
    Database = proplists:get_value(database, Args1),
    Username = proplists:get_value(username, Args1),
    Password = proplists:get_value(password, Args1),
    Port = proplists:get_value(port, Args1, 5432),
    Ssl = proplists:get_value(ssl, Args1, false),
    SslOpts = proplists:get_value(ssl_opts, Args1, []),
    TcpOpts = proplists:get_value(tcp_opts, Args1, []),
    Timeout = proplists:get_value(timeout, Args1, 5000),

    Opts =
          #{
            host => Hostname,
            username => Username,
            password => Password, 
            database => Database,
            port => Port,
            timeout => Timeout,
            ssl => Ssl,
            ssl_opts => SslOpts,
            tcp_opts => TcpOpts
           },
    Opts1 = case proplists:is_defined(socket_active, Args1) of
                true -> Opts#{socket_active => proplists:get_value(socket_active, Args1)};
                _ -> Opts
            end,
    Opts2 = case proplists:is_defined(async, Args1) of
                true -> Opts1#{async => proplists:get_value(async, Args1)};
                _ -> Opts1
            end,
    Opts3 = case proplists:is_defined(codecs, Args1) of
                true -> Opts2#{codecs => proplists:get_value(codecs, Args1)};
                _ -> Opts2
            end,
    Opts4 = case proplists:is_defined(nulls, Args1) of
                true -> Opts3#{nulls => proplists:get_value(nulls, Args1)};
                _ -> Opts3
            end,
    Opts5 = case proplists:is_defined(replication, Args1) of
                true -> Opts4#{replication => proplists:get_value(replication, Args1)};
                _ -> Opts4
            end,

    {ok, Conn} = epgsql:connect(Opts5),
    {ok, #state{conn = Conn}}.

handle_call({squery, Sql}, _From, #state{conn = Conn} = State) ->
    {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{conn = Conn} = State) ->
    {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    ok = epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
