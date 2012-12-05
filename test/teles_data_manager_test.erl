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


get_agents_test_() ->
    {setup,
    fun setup/0,
    fun cleanup/1,
    [fun() ->
        ?_test(begin
            Pids = teles_data_manager:get_agents(test),
            ?assertEqual(2, length(Pids))
        end)
    end,
    fun() ->
        ?_test(begin
            [A, B] = teles_data_manager:get_agents(test),
            ?assertEqual(A, teles_data_manager:get_agent(test)),
            ?assertEqual(B, teles_data_manager:get_agent(test)),
            ?assertEqual(A, teles_data_manager:get_agent(test))
        end)
    end
    ]}.


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


object_test_() ->
    {setup,
    fun setup/0,
    fun cleanup/1,
    [fun() ->
        ?_test(begin
            [] = teles_data_manager:list_objects(test),
            ok = teles_data_manager:add_object(test, foo, bar),
            [foo] = teles_data_manager:list_objects(test),
            ok = teles_data_manager:delete_object(test, foo),
            [] = teles_data_manager:list_objects(test)
        end)
    end
    ]}.


association_test_() ->
    {setup,
    fun setup/0,
    fun cleanup/1,
    [fun() ->
        ?_test(begin
            ok = teles_data_manager:add_object(test, foo, bar),
            {ok, foo, bar, []} = teles_data_manager:list_associations(test, foo),

            ok = teles_data_manager:associate(test, foo, 47.1, 120.2, a),
            {ok, foo, bar, Geos} = teles_data_manager:list_associations(test, foo),
            [Geo] = Geos,

            RG = teles_data_agent:make_geo(47.1, 120.2),
            GID = RG#geometry.value,
            ?assertEqual({GID, a, RG}, Geo),

            ok = teles_data_manager:disassociate(test, foo, GID),
            {ok, foo, bar, []} = teles_data_manager:list_associations(test, foo)
        end)
    end
    ]}.



