-module(teles_conn_manager_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Start no children, as they are added dynamically
    % when needed
    {ok, {{one_for_one, 10, 10}, []}}.

