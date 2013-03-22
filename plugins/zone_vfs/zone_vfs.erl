-module(zone_vfs).

-export([zone_vfs/0]).

zone_vfs() ->
    Data = [{Id, Field, to_number(Value)} ||
               [_, Id,_,Field, Value] <-
                   [re:split(L, ":|\\t") ||
                       L <- re:split(os:cmd("kstat  -p -m zone_vfs | grep -v zone_vfs:0:"), "\n")],
               Field =/= <<"class">>,
               Field =/= <<"zonename">>],
    build_obj(Data, zone_map(), []).

build_obj([{Id, Field, Value} | R], ZoneMap, Data) ->
    UUID = proplists:get_value(Id, ZoneMap),
    Data1 = jsxd:thread([{set, [UUID, <<"event">>], <<"vfs">>},
                         {set, [UUID, <<"data">>, Field], Value}],
                        Data),
    build_obj(R, ZoneMap, Data1);

build_obj([], _, Data) ->
    Data.

zone_map() ->
    [_ | R] = re:split(os:cmd("zoneadm list -p"), "\n"),
    [{Id, UUID} || [Id, UUID | _] <- [re:split(L, ":") || L <- R, L =/= <<>>]].


to_number(N) ->
    case re:run(N, "\\.") of
        nomatch ->
            list_to_integer(binary_to_list(N));
        _ ->
            list_to_float(binary_to_list(N))
    end.
