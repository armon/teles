-module(teles_acceptor_sup).
-behaviour(supervisor).
-export([start_link/2, init/1]).

start_link(Port, PoolSize) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, PoolSize]).

init([Port, PoolSize]) ->
    % Try to start listening
    {ok, Listen} = gen_tcp:listen(Port, [binary,
                                           {reuseaddr,true},
                                           {active,false},
                                           {backlog,128}]),

    % Get a pool of acceptors
    Pool = pool(Listen, PoolSize, []),
    {ok, {{one_for_one, 10, 10}, Pool}}.

pool(_, 0, Workers) -> Workers;
pool(Listen, PoolSize, Workers) ->
    Spec = {{acceptor, PoolSize}, {teles_acceptor, start_link, [Listen]},
            permanent, 5000, worker, dynamic},
    pool(Listen, PoolSize - 1, [Spec | Workers]).

