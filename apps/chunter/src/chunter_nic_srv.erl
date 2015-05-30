-module(chunter_nic_srv).

-export([get_vnic/1]).

get_vnic(Network) ->
    NIC = find_vnic(),
    os:cmd(["dladm create-vnic -t -l ", nic_for(Network), " ", NIC]),
    list_to_binary(NIC).


%% TODO: Nope not going to work like that.
nic_for(_) ->
    "bge0".

find_vnic() ->
    find_vnic(0, vnics()).
find_vnic(I, Nics) ->
    ID = integer_to_list(I),
    NIC = "net" ++ ID,
    case lists:member(NIC, Nics) of
        true ->
            find_vnic(I + 1, Nics);
        false ->
            NIC
    end.

vnics() ->
    [N || N <- re:split(os:cmd("/usr/sbin/dladm show-vnic -p -olink"), "\n"),
          N =/= <<>>].
