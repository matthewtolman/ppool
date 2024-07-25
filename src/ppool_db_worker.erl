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
    Args1 = ppool_conf:standardize_conf(Args),
    process_flag(trap_exit, true),
    Hostname = ppool_conf:get_conf_value(string, hostname, Args1),
    Database = ppool_conf:get_conf_value(string, database, Args1),
    Username = ppool_conf:get_conf_value(string, username, Args1),
    Password = ppool_conf:get_conf_value(string, password, Args1),
    Port = ppool_conf:get_conf_value(integer, port, Args1, 5432),
    Ssl = ppool_conf:get_conf_value(atom, ssl, Args1, false),
    SslOpts = ppool_conf:get_conf_value(list, ssl_opts, Args1, []),
    TcpOpts = ppool_conf:get_conf_value(list, tcp_opts, Args1, []),
    Timeout = ppool_conf:get_conf_value(integer, timeout, Args1, 5000),

    Opts =
        #{host => Hostname,
          username => Username,
          password => Password,
          database => Database,
          port => Port,
          timeout => Timeout,
          ssl => Ssl,
          ssl_opts => SslOpts,
          tcp_opts => TcpOpts},
    Opts1 =
        case ppool_conf:is_defined(socket_active, Args1) of
            true ->
                SocketActive = ppool_conf:get_conf_value(boolean, socket_active, Args1),
                case SocketActive of
                    [] -> false;
                    V when is_boolean(V) -> V;
                    V when is_integer(V) -> V;
                    "true" -> true;
                    "false" -> false;
                    S -> list_to_integer(S)
                end,
                Opts#{socket_active => SocketActive};
            _ ->
                Opts
        end,
    Opts2 =
        case ppool_conf:is_defined(async, Args1) of
            true ->
                Opts1#{async => ppool_conf:get_conf_value(atom, async, Args1)};
            _ ->
                Opts1
        end,
    Opts3 =
        case ppool_conf:is_defined(codecs, Args1) of
            true ->
                Opts2#{codecs => ppool_conf:get_conf_value(proplist, codecs, Args1)};
            _ ->
                Opts2
        end,
    Opts4 =
        case ppool_conf:is_defined(nulls, Args1) of
            true ->
                Opts3#{nulls => ppool_conf:get_conf_value(list, nulls, Args1)};
            _ ->
                Opts3
        end,
    Opts5 =
        case ppool_conf:is_defined(replication, Args1) of
            true ->
                Opts4#{replication => ppool_conf:get_conf_value(string, replication, Args1)};
            _ ->
                Opts4
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
