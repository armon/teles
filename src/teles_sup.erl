-module(teles_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % Collect all the variables first
    {ok, Port} = application:get_env(teles_port),
    {ok, AcceptPool} = application:get_env(teles_accept_pool),

    % Accept Manager, needs port and pool size
    AcceptManager  = {acceptors,
           {teles_acceptor_sup, start_link, [Port, AcceptPool]},
           permanent, 60000, supervisor, dynamic},

    {ok, { {one_for_one, 10, 10}, [AcceptManager]} }.

