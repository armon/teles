-module(teles_handler_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("rstar/include/rstar.hrl").

-record(state, {
            socket,
            space,
            buffer=[]
        }).

setup() -> #state{socket=sock}.
cleanup(_) -> ok.


process_buffer_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(S) ->
        ?_test(begin
            Blank = <<>>,
            Expect = S#state{buffer=Blank},
            ?assertEqual(Expect, teles_handler:process_buffer(S, Blank))
        end)
    end,
    fun(S) ->
        ?_test(begin
            Line = <<"not a whole line">>,
            Expect = S#state{buffer=Line},
            ?assertEqual(Expect, teles_handler:process_buffer(S, Line))
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, list_spaces,
                      [], {return, ["tubez"]}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [["tubez", <<"\n">>]], <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"list spaces\n">>,
            Blank = <<>>,
            Expect = S#state{buffer=Blank},
            ?assertEqual(Expect, teles_handler:process_buffer(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, list_spaces,
                      [], {return, []}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [], <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"list spaces\nafter">>,
            Blank = <<"after">>,
            Expect = S#state{buffer=Blank},
            ?assertEqual(Expect, teles_handler:process_buffer(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, list_spaces,
                      [], {return, []}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [], <<"END\n">>]]),
            em:strict(M, teles_data_manager, list_spaces,
                      [], {return, []}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [], <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"list spaces\r\nlist spaces\nafter">>,
            Blank = <<"after">>,
            Expect = S#state{buffer=Blank},
            ?assertEqual(Expect, teles_handler:process_buffer(S, Line)),

            em:verify(M)
        end)
    end
    ]}.


process_cmd_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Unrecognized command\n">>]),
            ok = em:replay(M),

            Line = <<"tubez">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, list_spaces,
                      [], {return, [<<"tubez">>]}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"Switched to space: ">>, <<"tubez">>, <<"\n">>]]),
            ok = em:replay(M),

            Line = <<"use space tubez">>,
            T = <<"tubez">>,
            Expect = S#state{space=T},
            ?assertEqual(Expect, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, list_spaces,
                      [], {return, [<<"tubez">>]}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Space does not exist\n">>]),
            ok = em:replay(M),

            Line = <<"use space bar">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, create_space,
                      [<<"foo">>], {return, ok}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Done\n">>]),
            ok = em:replay(M),

            Line = <<"create space foo">>,
            T = <<"foo">>,
            Expect = S#state{space=T},
            ?assertEqual(Expect, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, delete_space,
                      [<<"foo">>], {return, ok}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Done\n">>]),
            ok = em:replay(M),

            Line = <<"delete space foo">>,
            T = <<"foo">>,
            In = S#state{space=T},
            Expect = S#state{space=undefined},
            ?assertEqual(Expect, teles_handler:process_cmd(In, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, delete_space,
                      [<<"foo">>], {return, not_found}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Space does not exist\n">>]),
            ok = em:replay(M),

            Line = <<"delete space foo">>,
            T = <<"foo">>,
            In = S#state{space=T},
            ?assertEqual(In, teles_handler:process_cmd(In, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, list_spaces,
                      [], {return, [<<"test">>]}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [[<<"test">>, <<"\n">>]], <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"list spaces">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, list_objects,
                      [<<"test">>], {return, {ok, [<<"foobar">>]}}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [[<<"foobar">>, <<"\n">>]], <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"in test list objects">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Must use a namespace\n">>]),
            ok = em:replay(M),

            Line = <<"list objects">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, add_object,
                      [<<"test">>, <<"foo">>, undefined], {return, ok}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Done\n">>]),
            ok = em:replay(M),

            Line = <<"in test add object foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, delete,
                      [<<"test">>, <<"foo">>], {return, ok}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Done\n">>]),
            ok = em:replay(M),

            Line = <<"in test delete object foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, delete,
                      [<<"test">>, <<"foo">>], {return, {error, not_found, []}}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Object not found\n">>]),
            ok = em:replay(M),

            Line = <<"in test delete object foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, list_objects,
                      [<<"test">>], {return, {ok, [<<"bar">>]}}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [[<<"bar">>, <<"\n">>]], <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"in test list objects">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, associate,
                      [<<"test">>, <<"foo">>, 40.5, -120.5, undefined],
                      {return, ok}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Done\n">>]),
            ok = em:replay(M),

            Line = <<"in test associate point 40.5 -120.5 with foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, associate,
                      [<<"test">>, <<"foo">>, 40.5, -120.5, undefined],
                      {return, {error, not_found, []}}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Object not found\n">>]),
            ok = em:replay(M),

            Line = <<"in test associate point 40.5 -120.5 with foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
                em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            ok = em:replay(M),

            Line = <<"in test associate point crap -120.5 with foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
                em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            ok = em:replay(M),

            Line = <<"in test associate point 40.5 crap.5 with foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
                em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad arguments\n">>]),
            ok = em:replay(M),

            Line = <<"in test associate point 40.5 -120.5">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            G = rstar_geometry:point2d(40.5, 120.5, undefined),
            GID = erlang:phash2(G, 4294967295),
            G1 = G#geometry{value=GID},
            M = em:new(),
            em:strict(M, teles_data_manager, list_associations,
                      [<<"test">>, <<"foo">>],
                      {return, {ok, <<"foo">>, undefined, [{GID, undefined, G1}]}}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>,
                              [[[<<"GID=">>,integer_to_list(GID),<<" ">>,
                                 <<"lat=">>,["40", ".", "5000"], <<" ">>, <<"lng=">>,["120", ".", "5000"]], <<"\n">>]],
                        <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"in test list associations with foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, disassociate,
                      [<<"test">>, <<"foo">>, 1234],
                      {return, ok}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Done\n">>]),
            ok = em:replay(M),

            Line = <<"in test disassociate 1234 with foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, disassociate,
                      [<<"test">>, <<"foo">>, 1234],
                      {return, {error, not_found, []}}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Object not found\n">>]),
            ok = em:replay(M),

            Line = <<"in test disassociate 1234 with foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, teles_data_manager, disassociate,
                      [<<"test">>, <<"foo">>, 1234],
                      {return, {error, not_associated, []}}),
            em:strict(M, gen_tcp, send,
                      [sock, <<"GID not associated\n">>]),
            ok = em:replay(M),

            Line = <<"in test disassociate 1234 with foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad GID format\n">>]),
            ok = em:replay(M),

            Line = <<"in test disassociate tubez1234 with foo">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            Box = #geometry{dimensions=2, mbr=[{0.0, 20.0}, {-10.0, 30.0}]},
            M = em:new(),
            em:strict(M, teles_data_manager, query_within,
                      [<<"test">>, Box],
                      {return, {ok, [<<"foo">>]}}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [[<<"foo">>, <<"\n">>]], <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"in test query within 0 20 -10 30">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad arguments\n">>]),
            ok = em:replay(M),

            Line = <<"in test query within 0 20 -10">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            ok = em:replay(M),

            Line = <<"in test query within 0 20 -10 crap">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            Line1 = <<"in test query within 0 20 200 crap">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line1)),

            Line2 = <<"in test query within -100 20 -10 30">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line2)),

            Line3 = <<"in test query within 0 crap -10 30">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line3)),

            Line4 = <<"in test query within 40 10 30 20">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line4)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            Point = rstar_geometry:point2d(40.5, 120.5, undefined),
            M = em:new(),
            em:strict(M, teles_data_manager, query_nearest,
                      [<<"test">>, Point, 5],
                      {return, {ok, [<<"foo">>]}}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [[<<"foo">>, <<"\n">>]], <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"in test query nearest 5 to 40.5 120.5">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad K format\n">>]),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad K format\n">>]),
            ok = em:replay(M),

            Line = <<"in test query nearest 0 to 40.5 120.5">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            Line2 = <<"in test query nearest crap to 40.5 120.5">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line2)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            ok = em:replay(M),

            Line = <<"in test query nearest 5 to -100 120.5">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            Line2 = <<"in test query nearest 5 to 40.5 crap">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line2)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            Point = rstar_geometry:point2d(40.5, 120.5, undefined),
            M = em:new(),
            em:strict(M, teles_data_manager, query_around,
                      [<<"test">>, Point, 1000.0],
                      {return, {ok, [<<"foo">>]}}),
            em:strict(M, gen_tcp, send,
                      [sock, [<<"START\n">>, [[<<"foo">>, <<"\n">>]], <<"END\n">>]]),
            ok = em:replay(M),

            Line = <<"in test query around 40.5 120.5 for 1000m">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad lat/lng format\n">>]),
            ok = em:replay(M),

            Line = <<"in test query around -100 120.5 for 1000">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            Line2 = <<"in test query around 40.5 crap for 1000">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line2)),

            em:verify(M)
        end)
    end,
    fun(S) ->
        ?_test(begin
            M = em:new(),
            em:strict(M, gen_tcp, send,
                      [sock, <<"Client Error: Bad distance format\n">>]),
            ok = em:replay(M),

            Line = <<"in test query around -50 120.5 for 1000xyz">>,
            ?assertEqual(S, teles_handler:process_cmd(S, Line)),

            em:verify(M)
        end)
    end

    ]}.

