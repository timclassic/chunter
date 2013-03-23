-module(zone_memstat).

-export([zone_vfs/1]).

zone_vfs(KStat) ->
    Data = ekstat:read(KStat, "zone_memory_cap"),
    Data1 = [Keys || {_,_,_ID,_, Keys} <- Data, _ID =/= 0],
    Data2 = [[{list_to_binary(K), V} || {K, V} <- Vs,
                                        K =:= "zonename" orelse
                                            K =:= "rss" orelse
                                            K =:= "physcap" orelse
                                            K =:= "swap" orelse
                                            K =:= "swapcap"] || Vs <- Ks],
    build_obj(Data1, []).

build_obj([Keys | R], Data) ->
    UUID = list_to_binary(proplists:get_value(<<"zonename">>, Keys)),
    Statistics = lists:foldl(fun({<<"zonename">>, _}, Obj) ->
                                     Obj;
                                ({K, V}, Obj) ->
                                     jsxd:thread([K], V, Obj)
                             end, [], Keys),
    Data1 = jsxd:thread([{set, [UUID, <<"event">>], <<"memstat">>},
                         {set, [UUID, <<"data">>], Statistics}],
                        Data),
    build_obj(R, Data1);

build_obj([], Data) ->
    Data.
