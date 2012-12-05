-module(teles_data_manager_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("rstar/include/rstar.hrl").

setup() ->
    case teles_data_manager_sup:start_link(2) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

cleanup(_Pid) -> ok.

list_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(_) ->
        ?_test(begin
            ?assertEqual([], teles_data_manager:list_spaces())
        end)
    end,
    fun(_) ->
        ?_test(begin
            ?assertEqual(ok, teles_data_manager:create_space(test)),
            ?assertEqual([test], teles_data_manager:list_spaces())
        end)
    end
    ]}.

