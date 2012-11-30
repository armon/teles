-module(teles_handler).
-behavior(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
            socket,
            buffer=[]
        }).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


init([Socket]) ->
    State = #state{socket=Socket},
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


% Wait for the start message, set the socket to active
handle_cast(start, S=#state{socket=Socket}) ->
    inet:setopts(Socket, [{active, true}]),
    {noreply, S}.


% Store new data in the buffer
handle_info({tcp, _, Data}, State=#state{buffer=Buf}) ->
    NewBuf = [Buf, Data],
    lager:debug("Got: ~p~n", [Data]),
    {noreply, State#state{buffer=NewBuf}};


% Handle a close
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};


% Handle an error
handle_info({tcp_error, _Socket, Reason}, State) ->
    lager:warning("TCP Error on socket ~p. Err: ~p", [State, Reason]),
    {stop, normal, State}.


terminate(_Reason, _State) -> ok.

code_change(_OldVsvn, State, _Extra) -> {ok, State}.

