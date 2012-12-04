-module(teles_data_agent_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, Pid} = teles_data_agent:start_link(1, test), Pid.

cleanup(Pid) ->
    gen_server:call(Pid, stop).

list_test_() ->
    {setup,
    fun setup/0,
    fun cleanup/1,
    fun(Pid) ->
        ?_test(begin
            ?assertEqual({ok, []}, gen_server:call(Pid, list_objects))
        end)
    end
    }.

