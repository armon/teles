-module(teles_data_manager_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("rstar/include/rstar.hrl").

setup() ->
    case teles_data_manager_sup:start_link(2) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

cleanup(Pid) -> unlink(Pid), exit(Pid, kill).

list_test_() ->
    {setup,
    fun setup/0,
    fun cleanup/1,
    [fun() ->
        ?_test(begin
            ?assertEqual([], teles_data_manager:list_spaces())
        end)
    end,
    fun() ->
        ?_test(begin
            ?assertEqual(ok, teles_data_manager:create_space(test)),
            ?assertEqual([test], teles_data_manager:list_spaces())
        end)
    end,
    fun() ->
        ?_test(begin
            ?assertEqual(ok, teles_data_manager:create_space(test)),
            ?assertEqual(ok, teles_data_manager:delete_space(test)),
            ?assertEqual([], teles_data_manager:list_spaces())
        end)
    end
    ]}.

