-module(chunter_nic_srv).

-export([get_vnic/1, delete/1]).


%% [{<<"gateway">>,<<"10.0.0.1">>},
%%  {<<"ip">>,<<"10.0.0.103">>},
%%  {<<"netmask">>,<<"255.255.255.0">>},
%%  {<<"network_uuid">>,<<"503e27c5-061e-4f30-a4f5-c56104593ec5">>},
%%  {<<"nic_tag">>,<<"admin">>}]

delete(NicBin) ->
    Nic = binary_to_list(NicBin),
    Cmd = ["dladm delete-vnic ", Nic],
    lager:info("[dladm] ~s", [Cmd]),
    R = os:cmd(Cmd),
    lager:info("[dladm]-> ~s", [R]),
    ok.

get_vnic(Spec) ->
    {ok, Network} = jsxd:get(<<"nic_tag">>, Spec),
    {ok, IPBin} = jsxd:get(<<"ip">>, Spec),
    IP = binary_to_list(IPBin),
    {NIC, NICB} = find_vnic(),
    Props = [" -p allowed-ips=", IP, "/32"],
    Link = link_for(Network),
    Cmd = ["dladm create-vnic -l ", Link, " ", NIC, Props],
    lager:info("[dladm] ~s", [Cmd]),
    R = os:cmd(Cmd),
    lager:info("[dladm]-> ~s", [R]),

    {NICB, Spec}.

link_for(Network) when is_binary(Network) ->
    link_for(binary_to_list(Network));

link_for(Network) ->
    {ok, Networks} = application:get_env(chunter, network_tags),
    {Network, IFace} = lists:keyfind(Network, 1, Networks),
    IFace.

find_vnic() ->
    find_vnic(0, vnics()).

find_vnic(I, Nics) ->
    ID = integer_to_list(I),
    NIC = "net" ++ ID,
    NICB = list_to_binary(NIC),
    case lists:member(NICB, Nics) of
        true ->
            find_vnic(I + 1, Nics);
        false ->
            {NIC, NICB}
    end.

vnics() ->
    [N || N <- re:split(os:cmd("/usr/sbin/dladm show-vnic -p -olink"), "\n"),
          N =/= <<>>].


