-module(teles_data_agent).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        id,         % Agent ID
        space,      % Name of the space, e.g. 'cities'
        objects,    % ETS table of objects in the space
                    % Maps OID -> {Value, [#geometry]}
        rstar       % R*-tree index of objects
    }).

start_link(ID, Space) ->
    Name = list_to_atom(lists:flatten(io_lib:format("agent_~s_~p", [Space, ID]))),
    gen_server:start_link({local, Name}, ?MODULE, [ID, Space], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ID, Space]) ->
    ObjectTable = ets:new(objects, []),
    Rtree = rstar:new(2),
    State = #state{id=ID, space=Space, objects=ObjectTable, rstar=Rtree},
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

