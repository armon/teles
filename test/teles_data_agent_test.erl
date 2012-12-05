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

delete_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [fun(Pid) ->
        ?_test(begin
            ?assertEqual(not_found,
                         gen_server:call(Pid, {delete, tubez}))
        end)
    end,
    fun(Pid) ->
        ?_test(begin
            ?assertEqual(ok,
                gen_server:call(Pid, {add_object, tubez, 0})),
            ?assertEqual(ok,
                gen_server:call(Pid, {associate, tubez, 48.1, 120.2, foo})),
            ?assertEqual(ok,
                gen_server:call(Pid, {delete, tubez})),
            ?assertEqual(not_found,
                         gen_server:call(Pid, {delete, tubez}))
        end)
    end
    ]}.


insert(Pid, Name, Lat, Lng) ->
    ?assertEqual(ok,
        gen_server:call(Pid, {add_object, Name, 0})),
    ?assertEqual(ok,
        gen_server:call(Pid, {associate, Name, Lat, Lng, foo})).


query_test_() ->
    {foreach,
     fun() ->
        Pid = setup(),
        insert(Pid, tubez, 48.1, 120.2),
        insert(Pid, foo, -48.1, 120.2),
        insert(Pid, bar, 49.2, 120.2),
        insert(Pid, baz, 45.0, 120.2),
        Pid
     end,
    fun cleanup/1,
    [fun(Pid) ->
        ?_test(begin
            RG = teles_data_agent:make_geo(48.1, 120.2),
            ?assertEqual({ok, [tubez]},
                gen_server:call(Pid, {query_nearest, RG, 1})),
            ?assertEqual({ok, [bar, tubez]},
                gen_server:call(Pid, {query_nearest, RG, 2}))
        end)
    end,
    fun(Pid) ->
        ?_test(begin
            RG = teles_data_agent:make_geo(48.1, 120.2),
            ?assertEqual({ok, [tubez]},
                gen_server:call(Pid, {query_around, RG, 1})),
            ?assertEqual({ok, [bar, tubez]},
                gen_server:call(Pid, {query_around, RG, 140000}))
        end)
    end,
    fun(Pid) ->
        ?_test(begin
            Box = #geometry{dimensions=2, mbr=[{40, 50}, {100, 130}]},
            ?assertEqual({ok, [bar, tubez, baz]},
                gen_server:call(Pid, {query_within, Box})),

            Box2 = #geometry{dimensions=2, mbr=[{-50, 50}, {100, 130}]},
            ?assertEqual({ok, [bar, tubez, baz, foo]},
                gen_server:call(Pid, {query_within, Box2}))
        end)
    end
    ]}.

