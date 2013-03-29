-module(zone_net).

-export([zone_net/1]).

zone_net({KStat, Acc}) ->
    Data = ekstat:read(KStat, "net", "link"),
    Data1 = [[{"ifname", fix_name(Name)} | Keys] || {_,_,_ID, Name, Keys} <- Data],
    Data2 = [[{list_to_binary(K), V} || {K, V} <- Vs,
                                        K =:= "zonename" orelse
                                            K =:= "ifname" orelse
                                            K =:= "opackets64" orelse
                                            K =:= "obytes64" orelse
                                            K =:= "oerrors" orelse
                                            K =:= "ipackets64" orelse
                                            K =:= "rbytes64" orelse
                                            K =:= "ierrors"] || Vs <- Data1],
    {KStat, build_obj(Data2, Acc)}.

fix_name(Name) ->
    case re:split(Name, "_") of
        [Name1] ->
            Name1;
	[_ | Name1] ->
            iolist_to_binary(Name1)
    end.

build_obj([Keys | R], Data) ->
    case list_to_binary(proplists:get_value(<<"zonename">>, Keys)) of
        <<"global">> ->
            build_obj(R, Data);
        UUID ->
            Statistics = lists:foldl(fun({<<"zonename">>, _}, Obj) ->
                                             Obj;
                                        ({K, V}, Obj) ->
                                             jsxd:set([K], V, Obj)
                                     end, [], Keys),
            Data1 = [{UUID, [{<<"data">>, Statistics},
                             {<<"event">>, <<"net">>}]}|Data],
            build_obj(R, Data1)
    end;

build_obj([], Data) ->
    Data.
