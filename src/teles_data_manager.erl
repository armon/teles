-module(teles_data_manager).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        num_agents=1, % Number of agents per space
        agents        % Space -> [agent()]
    }).

start_link(NumAgents) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [NumAgents], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([NumAgents]) ->
    State = #state{num_agents=NumAgents, agents=dict:new()},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%%%% ------------------------------------------------------------------
%% External Function Definitions
%% ------------------------------------------------------------------


