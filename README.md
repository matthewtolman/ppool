ppool
=====

A Postgres database connection pool

Build
-----

    $ rebar3 compile

Docs
-----

    $ rebar3 edoc

Configuration
-----

Configuration can be passed through maps or proplists. App config files will be setting config for the `ppool` app. Configuration is in the following format:

```erlang
-type(resolver(t) :: {env, string()} | % get system environment variable
                    {fn, atom(), atom()} | % Call a method with 0 arity. Return should be either no_value or {value, Value}
                    {chain, [resolver(t)]} | % Chain of resolvers to evaluate in order
                    {value, t()}, % Set to a specific value (useful if it could collide with resolver values)
                    t()). % A value to resolve to

% Note: resolver(t) can only get the following types from environment variables:
%  integer(), string(), float(), atom(), binary(), boolean()
% Any other type must be provided as a term or through a function call

#{
    sup_name            => atom(),              % Optional, changes registered names for pool supervisor
    default_pool        => atom(),              % Optional, default pool to use with ppool_db calls
    id_siffix           => binary(),            % Optional, binary to add to end of table name to get ID column
                                                % Defaults to <<"_id">>
    use_inflection      => boolean(),           % Optional, determines if table names should be turned to singular
                                                % when determining column name (e.g. "users" => "user")
                                                % Default is true
    inflection_method   => {atom(), atom()},    % {Module, Method} to use for inflection
                                                % Defaults to {ppool_inflection, singular}
    pools => [
        #{
            name => atom(), % Required, name to register pool as
                            % Used when specifying the pool to ppool_db calls
            pool => #{
                size            => resolver(integer()),   % Optional, maximum connection pool size. Default 10
                max_overflow    => resolver(integer()),   % Optional, maximum number of connections created if pool is emtpy. Default 10
                strategy        => resolver(lifo | fifo)  % Optional, determines how freed connections are put back in the. Default lifo queue
            },
            db => #{
                hostname        => resolver(string() | binary()),                  % Required, DB host name
                database        => resolver(string() | binary()),                  % Required, database to connect to
                username        => resolver(string() | binary()),                  % Required, username to login as
                password        => resolver(string() | binary()),                  % Required, password to login with
                port            => resolver(integer()),                            % Optional, port to connect to (default 5432)
                ssl             => resolver(boolean() | 'required'),               % Optional, whether to use SSL
                ssl_opts        => resolver([ssl_option()]),                       % Optional, epgsql SSL params
                tcp_opts        => resolver([tcp_option()]),                       % Optional, epgsql TCP params
                socket_active   => resolver(boolean() | integer()),                % Optional, epgsql socket active option
                async           => resolver(atom() | pid()),                       % Optional, process to receive LISTEN/NOTIFY messages
                codecs          => resolver([{epgsql_codec:codec_mod(), any()}]),  % Optional, epgsql codec args
                nulls           => resolver([any()]),                              % Optional, epgsq NULL terms
                replication     => resolver(Replication :: string())               % Optional, replication string info
            }
        }
    ]
}
```

