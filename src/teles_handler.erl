-module(teles_handler).
-behavior(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
            socket,
            space,
            buffer=[]
        }).

-define(NEWLINE, <<"\n">>).
-define(DONE, <<"Done\n">>).
-define(START, <<"START\n">>).
-define(END, <<"END\n">>).
-define(OBJ_NOT_FOUND, <<"Object not found\n">>).


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
    NewBuf = iolist_to_binary([Buf, Data]),
    lager:info("Got: ~p~n", [Data]),
    NS = process_buffer(State, NewBuf),
    {noreply, NS};


% Handle a close
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};


% Handle an error
handle_info({tcp_error, _Socket, Reason}, State) ->
    lager:warning("TCP Error on socket ~p. Err: ~p", [State, Reason]),
    {stop, normal, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVsvn, State, _Extra) -> {ok, State}.

%%%
% Request processing
%%%

% Processes all commands in a buffer
process_buffer(State, Buffer) ->
    case binary:split(Buffer, [<<"\r\n">>, <<"\n">>]) of
        % No further commands can be processed, return remaining buffer
        [_] -> State#state{buffer=Buffer};

        % Process each available command
        [Cmd, Buf] ->
            S1 = process_cmd(State, Cmd),
            process_buffer(S1, Buf)
    end.


% Sets the associated space of a connection. Required for most commands
process_cmd(State=#state{socket=Sock}, <<"use space ", Space/binary>>) ->
    Spaces = teles_data_manager:list_spaces(),
    case lists:member(Space, Spaces) of
        true ->
            gen_tcp:send(Sock, [<<"Switched to space: ">>, Space, ?NEWLINE]),
            State#state{space=Space};
        false ->
            gen_tcp:send(Sock, <<"Space does not exist\n">>),
            State
    end;


% Creates a new space and switches into it
process_cmd(State=#state{socket=Sock}, <<"create space ", Space/binary>>) ->
    ok = teles_data_manager:create_space(Space),
    gen_tcp:send(Sock, ?DONE), State#state{space=Space};


% Deletes a space and leaves it if in use
process_cmd(State=#state{socket=Sock, space=Sp}, <<"delete space ", Space/binary>>) ->
    case teles_data_manager:delete_space(Space) of
        not_found ->
            gen_tcp:send(Sock, <<"Space does not exist\n">>),
            State;

        ok ->
            gen_tcp:send(Sock, ?DONE),

            % Leave the space if they are in the one they deleted
            case Sp of
                Space -> State#state{space=undefined};
                _ -> State
            end
    end;


% Lists all spaces
process_cmd(State=#state{socket=Sock}, <<"list spaces">>) ->
    Spaces = teles_data_manager:list_spaces(),
    send_list(Sock, Spaces), State;


% All other commands require a 'Space' to be used
process_cmd(State=#state{socket=Sock, space=undefined}, _) ->
    gen_tcp:send(Sock, <<"Client Error: Connection must use a namespace. Issue a 'use space' command first.\n">>), State;


% Adds a new object
process_cmd(State=#state{socket=Sock, space=Sp}, <<"add object ", OID/binary>>) ->
    % Don't support values for now
    ok = teles_data_manager:add_object(Sp, OID, undefined),
    gen_tcp:send(Sock, ?DONE), State;


% Deletes an object
process_cmd(State=#state{socket=Sock, space=Sp}, <<"delete object ", OID/binary>>) ->
    case teles_data_manager:delete(Sp, OID) of
        ok -> gen_tcp:send(Sock, ?DONE);
        {error, not_found, _} -> gen_tcp:send(Sock, ?OBJ_NOT_FOUND)
    end,
    State;


% Lists the existing objects
process_cmd(State=#state{socket=Sock, space=Sp}, <<"list objects">>) ->
    {ok, Objects} = teles_data_manager:list_objects(Sp),
    send_list(Sock, Objects), State;


% Associates an object with a new point
process_cmd(State=#state{socket=Sock, space=Sp}, <<"associate point ", Rest/binary>>) ->
    case binary:split(Rest, [<<" ">>], [global]) of
        [LatB, LngB, <<"with">>, Obj] ->
            % Convert lat and lng to floats
            Lat = to_float(LatB),
            Lng = to_float(LngB),

            % Check for proper types
            if
                not is_float(Lat) or not is_float(Lng) ->
                    gen_tcp:send(Sock, <<"Client Error: Bad Lat/Lng format\n">>);

                true ->
                    case teles_data_manager:associate(Sp, Obj, Lat, Lng, undefined) of
                        ok -> gen_tcp:send(Sock, ?DONE);
                        {error, not_found, _} -> gen_tcp:send(Sock, ?OBJ_NOT_FOUND)
                    end
            end;
        _ -> gen_tcp:send(Sock, <<"Client Error: Bad arguments\n">>)
    end,
    State;


process_cmd(State=#state{socket=Sock, space=Sp}, <<"list associations with ", _Rest/binary>>) ->
    gen_tcp:send(Sock, <<"Server Error: Unsupported command\n">>), State;

process_cmd(State=#state{socket=Sock, space=Sp}, <<"disassociate point ", _Rest/binary>>) ->
    gen_tcp:send(Sock, <<"Server Error: Unsupported command\n">>), State;

process_cmd(State=#state{socket=Sock, space=Sp}, <<"query within ", _Rest/binary>>) ->
    gen_tcp:send(Sock, <<"Server Error: Unsupported command\n">>), State;

process_cmd(State=#state{socket=Sock, space=Sp}, <<"query around ", _Rest/binary>>) ->
    gen_tcp:send(Sock, <<"Server Error: Unsupported command\n">>), State;

process_cmd(State=#state{socket=Sock, space=Sp}, <<"query nearest ", _Rest/binary>>) ->
    gen_tcp:send(Sock, <<"Server Error: Unsupported command\n">>), State;


% Catch all for an undefined command
process_cmd(State=#state{socket=Sock}, _) ->
    gen_tcp:send(Sock, <<"Client Error: Unrecognized command\n">>), State.


% Sends a list oriented response as
% START
% N1
% N2
% ..
% END
send_list(Sock, List) ->
    % Terminate each line
    Terminated = [[Line, ?NEWLINE] || Line <- List],
    gen_tcp:send(Sock, [?START, Terminated, ?END]).


-spec to_float(binary()) -> error | float().
to_float(Bin) ->
    try list_to_float(binary_to_list(Bin))
    catch
        _ -> error
    end.

