-module(teles_data_manager_sup).
-behaviour(supervisor).
-export([start_link/1, init/1, start_agents/2]).

start_link(NumAgents) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [NumAgents]).

init([NumAgents]) ->
    Manager = {data_manager, {teles_data_manager, start_link, [NumAgents]},
            permanent, 5000, worker, dynamic},

    % Start no agents, as they are added dynamically
    % when needed
    {ok, {{one_for_one, 10, 10}, [Manager]}}.


% Starts a number of agents
start_agents(Num, Space) ->
    start_agents(Num, Space, []).

start_agents(0, _, Results) -> Results;
start_agents(Num, Space, Results) ->
    AgentSpec = {{agent, Space, Num},
                {teles_data_agent, start_link, [Num, Space]},
                temporary, 5000, worker, dynamic},

    % Start the child
    Res = case supervisor:start_child(?MODULE, AgentSpec) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid;
        {error, _} -> error
    end,
    start_agents(Num - 1, Space, [Res | Results]).

