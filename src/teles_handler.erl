-module(teles_handler).
-behavior(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
            socket,
            space,
            buffer=[]
        }).

-include_lib("rstar/include/rstar.hrl").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(NEWLINE, <<"\n">>).
-define(DONE, <<"Done\n">>).
-define(START, <<"START\n">>).
-define(END, <<"END\n">>).
-define(SPACE, <<" ">>).
-define(OBJ_NOT_FOUND, <<"Object not found\n">>).
-define(BAD_LATLNG, <<"Client Error: Bad lat/lng format\n">>).
-define(BAD_ARGS, <<"Client Error: Bad arguments\n">>).
-define(SPACE_NOT_FOUND, <<"Space does not exist\n">>).
-define(SPACE_NEEDED(State), gen_tcp:send(State#state.socket, <<"Client Error: Must use a namespace\n">>), State).

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
            gen_tcp:send(Sock, ?SPACE_NOT_FOUND),
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
            gen_tcp:send(Sock, ?SPACE_NOT_FOUND),
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


% Executes a different command in a space
process_cmd(State=#state{socket=Sock}, <<"in ", Rest/binary>>) ->
    case binary:split(Rest, [<<" ">>]) of
        [Space, Cmd] ->
            % Store the current space
            CurSpace = State#state.space,

            % Invoke the command in the new space
            S1 = State#state{space=Space},
            S2 = process_cmd(S1, Cmd),

            % Restore the state
            S2#state{space=CurSpace};

        _ -> gen_tcp:send(Sock, ?BAD_ARGS), State
    end;


% Adds a new object
process_cmd(State=#state{space=undefined}, <<"add object ", _OID/binary>>) -> ?SPACE_NEEDED(State);
process_cmd(State=#state{socket=Sock, space=Sp}, <<"add object ", OID/binary>>) ->
    % Don't support values for now
    ok = teles_data_manager:add_object(Sp, OID, undefined),
    gen_tcp:send(Sock, ?DONE), State;


% Deletes an object
process_cmd(State=#state{space=undefined}, <<"delete object ", _OID/binary>>) -> ?SPACE_NEEDED(State);
process_cmd(State=#state{socket=Sock, space=Sp}, <<"delete object ", OID/binary>>) ->
    case teles_data_manager:delete(Sp, OID) of
        ok -> gen_tcp:send(Sock, ?DONE);
        {error, not_found, _} -> gen_tcp:send(Sock, ?OBJ_NOT_FOUND)
    end,
    State;


% Lists the existing objects
process_cmd(State=#state{space=undefined}, <<"list objects">>) -> ?SPACE_NEEDED(State);
process_cmd(State=#state{socket=Sock, space=Sp}, <<"list objects">>) ->
    {ok, Objects} = teles_data_manager:list_objects(Sp),
    send_list(Sock, Objects), State;


% Associates an object with a new point
process_cmd(State=#state{space=undefined}, <<"associate point ", _Rest/binary>>) -> ?SPACE_NEEDED(State);
process_cmd(State=#state{socket=Sock, space=Sp}, <<"associate point ", Rest/binary>>) ->
    case binary:split(Rest, [<<" ">>], [global]) of
        [LatB, LngB, <<"with">>, Obj] ->
            % Convert lat and lng to floats
            Lat = to_float(LatB),
            Lng = to_float(LngB),
            InvalidLatLng = invalid_lat(Lat) orelse invalid_lng(Lng),

            % Check for proper types
            if
                InvalidLatLng ->
                    gen_tcp:send(Sock, ?BAD_LATLNG);

                true ->
                    case teles_data_manager:associate(Sp, Obj, Lat, Lng, undefined) of
                        ok -> gen_tcp:send(Sock, ?DONE);
                        {error, not_found, _} -> gen_tcp:send(Sock, ?OBJ_NOT_FOUND)
                    end
            end;
        _ -> gen_tcp:send(Sock, ?BAD_ARGS)
    end,
    State;


% Lists the associations of an object
process_cmd(State=#state{space=undefined}, <<"list associations with ", _Obj/binary>>) -> ?SPACE_NEEDED(State);
process_cmd(State=#state{socket=Sock, space=Sp}, <<"list associations with ", Obj/binary>>) ->
    case teles_data_manager:list_associations(Sp, Obj) of
        not_found -> gen_tcp:send(Sock, ?OBJ_NOT_FOUND);
        {ok, Obj, _Val, Geos} ->
            Associations = [association_line(ID, Geo) ||{ID, _, Geo} <- Geos],
            send_list(Sock, Associations)
    end,
    State;


% Disassociates a GID with an Object
process_cmd(State=#state{space=undefined}, <<"disassociate ", _Rest/binary>>) -> ?SPACE_NEEDED(State);
process_cmd(State=#state{socket=Sock, space=Sp}, <<"disassociate ", Rest/binary>>) ->
    case binary:split(Rest, [<<" ">>], [global]) of
        [GIDS, <<"with">>, Obj] ->
            GID = to_integer(GIDS),
            if
                not is_integer(GID) ->
                    gen_tcp:send(Sock, <<"Client Error: Bad GID format\n">>);

                true ->
                    case teles_data_manager:disassociate(Sp, Obj, GID) of
                        ok -> gen_tcp:send(Sock, ?DONE);
                        {error, not_found, _} -> gen_tcp:send(Sock, ?OBJ_NOT_FOUND);
                        {error, not_associated, _} -> gen_tcp:send(Sock, <<"GID not associated\n">>)
                    end
            end;
        _ -> gen_tcp:send(Sock, ?BAD_ARGS)
    end,
    State;


% Queries within a search box
process_cmd(State=#state{space=undefined}, <<"query within ", _Rest/binary>>) -> ?SPACE_NEEDED(State);
process_cmd(State=#state{socket=Sock, space=Sp}, <<"query within ", Rest/binary>>) ->
    case binary:split(Rest, [<<" ">>], [global]) of
        [MinLatS, MaxLatS, MinLngS, MaxLngS] ->
            MinLat = to_float(MinLatS),
            MaxLat = to_float(MaxLatS),
            MinLng = to_float(MinLngS),
            MaxLng = to_float(MaxLngS),
            InvalidLatLng = invalid_lat(MinLat) orelse invalid_lat(MaxLat)
                            orelse invalid_lng(MinLng) orelse invalid_lng(MaxLng),

            if
                InvalidLatLng ->
                    gen_tcp:send(Sock, ?BAD_LATLNG);

                MinLat > MaxLat orelse MinLng > MaxLng ->
                    gen_tcp:send(Sock, ?BAD_LATLNG);

                true ->
                    Box = #geometry{dimensions=2, mbr=[{MinLat, MaxLat}, {MinLng, MaxLng}]},
                    {ok, Results} = teles_data_manager:query_within(Sp, Box),
                    send_list(Sock, Results)
            end;
        _ -> gen_tcp:send(Sock, ?BAD_ARGS)
    end,
    State;


% Query the nearest points
process_cmd(State=#state{space=undefined}, <<"query nearest ", _Rest/binary>>) -> ?SPACE_NEEDED(State);
process_cmd(State=#state{socket=Sock, space=Sp}, <<"query nearest ", Rest/binary>>) ->
    case binary:split(Rest, [<<" ">>], [global]) of
        [K_str, <<"to">>, LatS, LngS] ->
            K = to_integer(K_str),
            Lat = to_float(LatS),
            Lng = to_float(LngS),
            InvalidLatLng = invalid_lat(Lat) orelse invalid_lng(Lng),

            if
                InvalidLatLng ->
                    gen_tcp:send(Sock, ?BAD_LATLNG);

                not is_integer(K) orelse K < 1 ->
                    gen_tcp:send(Sock, <<"Client Error: Bad K format\n">>);

                true ->
                    Point = rstar_geometry:point2d(Lat, Lng, undefined),
                    {ok, Results} = teles_data_manager:query_nearest(Sp, Point, K),
                    send_list(Sock, Results)
            end;
        _ -> gen_tcp:send(Sock, ?BAD_ARGS)
    end,
    State;


% Query around a given point
process_cmd(State=#state{space=undefined}, <<"query around ", _Rest/binary>>) -> ?SPACE_NEEDED(State);
process_cmd(State=#state{socket=Sock, space=Sp}, <<"query around ", Rest/binary>>) ->
    case binary:split(Rest, [<<" ">>], [global]) of
        [LatS, LngS, <<"for">>, DistS] ->
            Lat = to_float(LatS),
            Lng = to_float(LngS),
            Dist = dist_to_float(DistS),
            InvalidLatLng = invalid_lat(Lat) orelse invalid_lng(Lng),

            if
                InvalidLatLng ->
                    gen_tcp:send(Sock, ?BAD_LATLNG);

                not is_float(Dist) ->
                    gen_tcp:send(Sock, <<"Client Error: Bad distance format\n">>);

                true ->
                    Point = rstar_geometry:point2d(Lat, Lng, undefined),
                    {ok, Results} = teles_data_manager:query_around(Sp, Point, Dist),
                    send_list(Sock, Results)
            end;
        _ -> gen_tcp:send(Sock, ?BAD_ARGS)
    end,
    State;


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
        error:_ -> case to_integer(Bin) of
            error -> error;
            Int -> float(Int)
        end
    end.


-spec to_integer(binary()) -> error | integer().
to_integer(Bin) ->
    try list_to_integer(binary_to_list(Bin))
    catch
        error:_ -> error
    end.


% Returns a single line of an assoication given
% a GID and Geo objet
association_line(ID, Geo) ->
    #geometry{mbr=[{Lat, _}, {Lng, _}]} = Geo,
    IDs = integer_to_list(ID),
    LatS = format_float(Lat),
    LngS = format_float(Lng),
    [<<"GID=">>, IDs, ?SPACE, <<"lat=">>, LatS, ?SPACE, <<"lng=">>, LngS].


% Checks if a latitude is invalid
invalid_lat(Lat) ->
    if
        not is_float(Lat) -> true;
        Lat < -90 orelse Lat > 90 -> true;
        true -> false
    end.


% Checks if a longitude is invalid
invalid_lng(Lng) ->
    if
        not is_float(Lng) -> true;
        Lng < -180 orelse Lng > 180 -> true;
        true -> false
    end.


% Tries to convert a distance to meters
dist_to_float(Dist) ->
    % Convert to a list
    DistL = binary_to_list(Dist),

    % Partition into the digits and units
    {DistDigR, Unit} = lists:partition(fun(Char) ->
        ((Char >= $0) and (Char =< $9)) orelse Char == $- orelse Char == $.
    end, DistL),

    % Ensure '.' is in DistDig, or add .0
    DistDig = case lists:member($., DistDigR) of
        true -> DistDigR;
        _ -> DistDigR ++ [$., $0]
    end,

    % Convert the distance to a float
    DistV = try list_to_float(DistDig)
    catch
        error:_ ->
            try list_to_integer(DistDig)
            catch
                error:_ -> error
            end
    end,

    % Switch on the units
    case DistV of
        error -> error;
        _ ->
            case Unit of
                [] -> DistV;
                "m" -> DistV;
                "km" -> DistV * 1000.0;
                "mi" -> DistV * 1609.0;
                "y"  -> DistV * 0.9144;
                "ft" -> DistV * 0.3048;
                _ -> error
            end
    end.


% Gives a nice base 10 representation of a float
format_float(Val) ->
    % Get the whole number part
    WholePart = trunc(Val),

    % Get the sub part
    SubPart = abs(trunc(Val * 10000)) rem 10000,

    % Convert to iolist
    [integer_to_list(WholePart), ".", integer_to_list(SubPart)].



-ifdef(TEST).

to_float_test() ->
    ?assertEqual(error, to_float(<<"tubez0">>)),
    ?assertEqual(1.0, to_float(<<"1.0">>)),
    ?assertEqual(1.0, to_float(<<"1">>)),
    ?assertEqual(-1.0, to_float(<<"-1">>)).

to_int_test() ->
    ?assertEqual(error, to_integer(<<"junk">>)),
    ?assertEqual(1, to_integer(<<"1">>)),
    ?assertEqual(-1, to_integer(<<"-1">>)).

invalid_lat_test() ->
    ?assertEqual(false, invalid_lat(45.0)),
    ?assertEqual(false, invalid_lat(-45.0)),
    ?assertEqual(true, invalid_lat(-90.01)),
    ?assertEqual(true, invalid_lat(0)),
    ?assertEqual(true, invalid_lat(tubez)),
    ?assertEqual(true, invalid_lat(90.01)).

invalid_lng_test() ->
    ?assertEqual(false, invalid_lng(45.0)),
    ?assertEqual(false, invalid_lng(-45.0)),
    ?assertEqual(true, invalid_lng(-1800.01)),
    ?assertEqual(true, invalid_lng(0)),
    ?assertEqual(true, invalid_lng(tubez)),
    ?assertEqual(true, invalid_lng(180.01)).

format_float_test() ->
    ?assertEqual(["-123", ".", "1234"], format_float(-123.123456)),
    ?assertEqual(["123", ".", "1234"], format_float(123.123456)).

distance_test() ->
    ?assertEqual(1000.0, dist_to_float(<<"1000">>)),
    ?assertEqual(1000.0, dist_to_float(<<"1000m">>)),
    ?assertEqual(1000.0, dist_to_float(<<"1km">>)),
    ?assertEqual(1609.0 * 2, dist_to_float(<<"2mi">>)),
    ?assertEqual(9144.0, dist_to_float(<<"10000y">>)),
    ?assertEqual(3048.0, dist_to_float(<<"10000ft">>)).

-endif.
