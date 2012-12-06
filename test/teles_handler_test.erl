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
    end
    ]}.

