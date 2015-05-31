-module(chunter_nic_srv).

-export([get_vnic/1]).

get_vnic(Network) ->
    {NIC, NICB} = find_vnic(),
    os:cmd(["dladm create-vnic -t -l ", nic_for(Network), " ", NIC]),
    NICB.


%% TODO: Nope not going to work like that.
nic_for(_) ->
    "bge0".

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
