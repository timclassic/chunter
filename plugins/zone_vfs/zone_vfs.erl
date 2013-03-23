-module(zone_vfs).

-export([zone_vfs/1]).

zone_vfs(KStat) ->
    Data = ekstat:read(KStat, "zone_vfs", "zone_vfs"),
    Data1 = [{Id, Keys} || {_,_,ID,_, Keys} <- Data, ID <> 0],
    build_obj(Data, []).

build_obj([{Id, Keys} | R], Data) ->
    UUID = list_to_binary(proplists:get_value("zonename", Keys)),
    Statistics = lists:fold(fun({"zonename", _}, Obj) ->
                                    Obj;
                               ({K, V}, Obj) ->
                                    jsxd:set([list_to_binary(Key)], V, Obj)
                            end, [], Keys),

    Data1 = jsxd:thread([{set, [UUID, <<"event">>], <<"vfs">>},
                         {set, [UUID, <<"data">>], Statistics}],
                        Data),
    build_obj(R, ZoneMap, Data1);

build_obj([], _, Data) ->
    Data.
