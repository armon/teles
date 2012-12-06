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

