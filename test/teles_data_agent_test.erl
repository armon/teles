-module(teles_data_agent_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("rstar/include/rstar.hrl").

setup() ->
    {ok, Pid} = teles_data_agent:start_link(1, test), Pid.

cleanup(Pid) ->
    gen_server:call(Pid, stop).

list_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(Pid) ->
        ?_test(begin
            ?assertEqual({ok, []}, gen_server:call(Pid, list_objects))
        end)
    end,
    fun(Pid) ->
        ?_test(begin
            ?assertEqual(ok, gen_server:call(Pid, {add_object, tubez, 0})),
            ?assertEqual({ok, [tubez]}, gen_server:call(Pid, list_objects))
        end)
    end
    ]}.

list_no_assciation_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(Pid) ->
        ?_test(begin
            ?assertEqual(not_found,
                         gen_server:call(Pid, {list_associations, tubez}))
        end)
    end,
    fun(Pid) ->
        ?_test(begin
            ?assertEqual(ok,
                gen_server:call(Pid, {add_object, tubez, 0})),
            ?assertEqual(ok,
                gen_server:call(Pid, {associate, tubez, 48.1, 120.2, foo})),

            RG = teles_data_agent:make_geo(48.1, 120.2),
            GID = RG#geometry.value,
            ?assertEqual({ok, tubez, 0, [{GID, foo, RG}]},
                gen_server:call(Pid, {list_associations, tubez}))
        end)
    end
    ]}.

disassociate_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(Pid) ->
        ?_test(begin
            ?assertEqual(not_found,
                         gen_server:call(Pid, {disassociate, tubez, foo}))
        end)
    end,
    fun(Pid) ->
        ?_test(begin
            ?assertEqual(ok,
                gen_server:call(Pid, {add_object, tubez, 0})),
            ?assertEqual(ok,
                gen_server:call(Pid, {associate, tubez, 48.1, 120.2, foo})),

            RG = teles_data_agent:make_geo(48.1, 120.2),
            GID = RG#geometry.value,
            ?assertEqual(ok,
                gen_server:call(Pid, {disassociate, tubez, GID})),

            ?assertEqual({ok, tubez, 0, []},
                gen_server:call(Pid, {list_associations, tubez}))
        end)
    end
    ]}.

