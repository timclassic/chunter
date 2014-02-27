-module(zone_memstat).

-export([zone_memstat/1]).

zone_memstat({KStat, Acc}) ->
    Data = ekstat:read(KStat, "zone_memory_cap"),
    Data1 = [Keys || {_,_,_ID,_, Keys} <- Data, _ID =/= 0],
    Data2 = [[{list_to_binary(K), V} || {K, V} <- Vs,
                                        K =:= "zonename" orelse
                                            K =:= "rss" orelse
                                            K =:= "physcap" orelse
                                            K =:= "swap" orelse
                                            K =:= "swapcap"] || Vs <- Data1, is_list(Vs)],
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
                             {<<"event">>, <<"memstat">>}]}|Data],
            build_obj(R, Data1)
    end;

build_obj([], Data) ->
    Data.
