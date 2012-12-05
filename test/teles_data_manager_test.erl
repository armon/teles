-module(teles_data_manager_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("rstar/include/rstar.hrl").

setup() ->
    error_logger:tty(false),
    case teles_data_manager_sup:start_link(2) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

cleanup(Pid) -> unlink(Pid), exit(Pid, kill), timer:sleep(1).


get_agents_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(_) ->
        ?_test(begin
            Pids = teles_data_manager:get_agents(test),
            ?assertEqual(2, length(Pids))
        end)
    end,
    fun(_) ->
        ?_test(begin
            [A, B] = teles_data_manager:get_agents(test),
            ?assertEqual(A, teles_data_manager:get_agent(test)),
            ?assertEqual(B, teles_data_manager:get_agent(test)),
            ?assertEqual(A, teles_data_manager:get_agent(test))
        end)
    end
    ]}.


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
    end,
    fun(_) ->
        ?_test(begin
            ?assertEqual(ok, teles_data_manager:create_space(test)),
            ?assertEqual(ok, teles_data_manager:delete_space(test)),
            ?assertEqual([], teles_data_manager:list_spaces())
        end)
    end
    ]}.


object_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(_) ->
        ?_test(begin
            {ok, []} = teles_data_manager:list_objects(test),
            ok = teles_data_manager:add_object(test, foo, bar),
            {ok, [foo]} = teles_data_manager:list_objects(test),
            ok = teles_data_manager:delete(test, foo),
            {ok, []} = teles_data_manager:list_objects(test)
        end)
    end
    ]}.


association_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(_) ->
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


query_test_() ->
    {foreach,
    fun() ->
        Res = setup(),
        ok = teles_data_manager:add_object(test, foo, bar),
        ok = teles_data_manager:associate(test, foo, 47.1, 120.2, a),

        ok = teles_data_manager:add_object(test, bar, bar),
        ok = teles_data_manager:associate(test, bar, 47.2, 120.3, a),

        ok = teles_data_manager:add_object(test, baz, bar),
        ok = teles_data_manager:associate(test, baz, 47.2, 120.4, a),
        Res
    end,
    fun cleanup/1,
    [fun(_) ->
        ?_test(begin
            P = rstar_geometry:point2d(47, 120, undefined),
            Res = teles_data_manager:query_nearest(test, P, 3),
            ?assertEqual({ok, [bar, baz, foo]}, Res)
        end)
    end,
    fun(_) ->
        ?_test(begin
            P = rstar_geometry:point2d(47, 120, undefined),
            Res = teles_data_manager:query_around(test, P, 0.4),
            ?assertEqual({ok, [bar, foo]}, Res)
        end)
    end,
    fun(_) ->
        ?_test(begin
            Box = #geometry{dimensions=2,
                        mbr=[{47,48}, {120,121}]},
            Res = teles_data_manager:query_within(test, Box),
            ?assertEqual({ok, [bar, baz, foo]}, Res)
        end)
    end
    ]}.


recovery_test_() ->
    {foreach,
    fun() ->
        Res = setup(),
        ok = teles_data_manager:add_object(test, foo, bar),
        ok = teles_data_manager:associate(test, foo, 47.1, 120.2, a),

        ok = teles_data_manager:add_object(test, bar, bar),
        ok = teles_data_manager:associate(test, bar, 47.2, 120.3, a),

        ok = teles_data_manager:add_object(test, baz, bar),
        ok = teles_data_manager:associate(test, baz, 47.2, 120.4, a),
        Res
    end,
    fun cleanup/1,
    [fun(_) ->
        ?_test(begin
            [A, _] = teles_data_manager:get_agents(test),

            % Kill A
            gen_server:cast(A, whoops),
            timer:sleep(100),

            % Data should be saved
            ?assertEqual({ok, [foo, baz, bar]},
                         teles_data_manager:list_objects(test)),
            ?assertEqual({ok, [foo, baz, bar]},
                         teles_data_manager:list_objects(test)),
            ?assertEqual({ok, [foo, baz, bar]},
                         teles_data_manager:list_objects(test))
        end)
    end,
    fun(_) ->
        ?_test(begin
            [A, B] = teles_data_manager:get_agents(test),

            % Kill both
            gen_server:cast(A, whoops),
            gen_server:cast(B, whoops),
            timer:sleep(100),

            % Data should be lost
            ?assertEqual({ok, []},
                         teles_data_manager:list_objects(test)),
            ?assertEqual({ok, []},
                         teles_data_manager:list_objects(test)),
            ?assertEqual({ok, []},
                         teles_data_manager:list_objects(test))
        end)
    end

    ]}.



