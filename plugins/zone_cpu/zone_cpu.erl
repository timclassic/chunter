-module(zone_cpu).

-export([zone_cpu/1]).

zone_cpu({KStat, Acc}) ->
    Data = ekstat:read(KStat, "zone_caps", "caps"),
    Data1 = [Keys || {_,_,_ID, "cpucaps_zone_" ++ _, Keys} <- Data],
    Data2 = [[{list_to_binary(K), V} || {K, V} <- Vs,
                                        K =:= "zonename" orelse
                                            K =:= "usage" orelse
                                            K =:= "value" orelse
                                            K =:= "nwait" orelse
                                            K =:= "baseline" orelse
                                            K =:= "effective" orelse
                                            K =:= "maxusage" orelse
                                            K =:= "bursting_sec"
             ] || Vs <- Data1, is_list(Vs)],
    {KStat, build_obj(Data2, Acc)}.

build_obj([Keys | R], Data) ->
    case proplists:get_value(<<"zonename">>, Keys) of
        undefined ->
            build_obj(R, Data);
        UUIDs ->
            UUID = list_to_binary(UUIDs),
            Statistics = lists:foldl(fun({<<"zonename">>, _}, Obj) ->
                                             Obj;
                                        ({K, V}, Obj) ->
                                             jsxd:set([K], V, Obj)
                                     end, [], Keys),
            Data1 = [{UUID, [{<<"data">>, Statistics},
                             {<<"event">>, <<"cpu">>}]}|Data],
            build_obj(R, Data1)
    end;

build_obj([], Data) ->
    Data.
