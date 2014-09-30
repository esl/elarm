%%%-------------------------------------------------------------------
%%% @author Richard Jonas <richard.jonas@erlang-solutions.com>
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%%-------------------------------------------------------------------
-module(elarm_subscr).

-include_lib("elarm/include/elarm.hrl").
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [
      {"list servers test (internal)", fun which_servers_internal/0},
      {"list servers test", fun which_servers/0},
      {"subscribe test", fun subscribe/0},
      {"raise/clear test", fun raise_clear/0},
      {"raise/clear with reason test", fun raise_clear_with_reason/0},
      {"ack/unack test", fun raise_ack_unack/0},
      {"comment test", fun comment/0},
      {"manual clear test", fun manual_clear/0},
      {"subscribe summary test", fun subscribe_summary/0}
     ]}.

setup() ->
    ok = application:start(elarm),
    {ok, _} = elarm:start_server(elarm1, []).

teardown(_) ->
    ok = elarm:stop_server(elarm1),
    ok = application:stop(elarm).

which_servers_internal() ->
    ?assertMatch([{elarm1,_}, {elarm_server,_}],
                 lists:sort(elarm_man_sup_sup:which_servers())).

which_servers() ->
    ?assertMatch([{elarm1,_}, {elarm_server,_}],
                 lists:sort(elarm:which_servers())).

subscribe() ->
    {ok, Ref, _} = elarm:subscribe([all], self()),
    elarm:raise(diskfull, "/dev/hda1", []),
    receive
        {elarm, _, #alarm{alarm_id = diskfull, src = "/dev/hda1"}} ->
            ok
    end,
    elarm:unsubscribe(Ref).

raise_clear() ->
    {ok, Ref, _} = elarm:subscribe([all], self()),
    elarm:raise(too_hot, "cpu1", []),
    receive
        {elarm, _, #alarm{alarm_id = too_hot, src = "cpu1"}} ->
            ok
    end,

    elarm:clear(too_hot, "cpu1"),
    receive
        {elarm, _, {clear, too_hot, "cpu1", _, _Reason = ok}} ->
            ok
    end,

    {ok, Alarms} = elarm:get_alarms(),
    lists:all(fun(#alarm{alarm_id = Id, src = Src}) ->
                      not(Id =:= too_hot andalso Src =:= "cpu1")
              end, Alarms),
    elarm:unsubscribe(Ref).

raise_clear_with_reason() ->
    {ok, Ref, _} = elarm:subscribe([all], self()),
    elarm:raise(too_hot, "cpu1", []),
    receive
        {elarm, _, #alarm{alarm_id = too_hot, src = "cpu1"}} ->
            ok
    end,

    elarm:clear(elarm_server, too_hot, "cpu1", source_gone),
    receive
        {elarm, _, {clear, too_hot, "cpu1", _, _Reason = source_gone}} ->
            ok
    end,

    {ok, Alarms} = elarm:get_alarms(),
    lists:all(fun(#alarm{alarm_id = Id, src = Src}) ->
                      not(Id =:= too_hot andalso Src =:= "cpu1")
              end, Alarms),
    elarm:unsubscribe(Ref).

raise_ack_unack() ->
    {ok, Ref, _} = elarm:subscribe([all], self()),
    elarm:raise(fridge_melting, "fridge1", []),

    elarm:acknowledge(fridge_melting, "fridge1", <<"superuser">>),
    receive
        {elarm, _, {ack, fridge_melting, "fridge1", _,
                    #ack_info{user = <<"superuser">>}}} ->
            ok
    after
        100 ->
            ?assert(false)
    end,

    elarm:unacknowledge(fridge_melting, "fridge1", <<"superuser">>),
    receive
        {elarm, _, {unack, fridge_melting, "fridge1", _,
                    #ack_info{user = <<"superuser">>}}} ->
            ok
    after
        100 ->
            ?assert(false)
    end,

    elarm:unsubscribe(Ref).

comment() ->
    {ok, Ref, _} = elarm:subscribe([all], self()),
    elarm:raise(house_burning, "this one", []),
    elarm:add_comment(house_burning, "this one", <<"Who cares?">>,
                      <<"lazy joe">>),
    receive
        {elarm, _, {add_comment, house_burning, "this one", _,
                    #comment{user = <<"lazy joe">>,
                             text = <<"Who cares?">>}}} ->
            ok
    after
        100 ->
            ?assert(false)
    end,

    elarm:unsubscribe(Ref).

manual_clear() ->
    {ok, Ref, _} = elarm:subscribe([all], self()),
    elarm:raise(bad_disk, "sda", []),
    elarm:manual_clear(bad_disk, "sda", <<"admin">>),

    receive
        {elarm, _, {clear, bad_disk, "sda", _, {manual, <<"admin">>}}} ->
            ok
    end,

    elarm:unsubscribe(Ref).

subscribe_summary() ->
    {ok, Ref} = elarm:subscribe_summary([all]),
    receive
        {elarm, Ref, #alarm_summary{}} ->
            ok
    end,
    ok.
