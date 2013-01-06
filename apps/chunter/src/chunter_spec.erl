%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------

-module(chunter_spec).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([to_vmadm/3,
         to_sniffle/1]).


-spec to_vmadm(Package::fifo:package(), Dataset::fifo:dataset(), OwnerData::fifo:config()) -> fifo:vm_config().

to_vmadm(Package, Dataset, OwnerData) ->
    case lists:keyfind(<<"type">>, 1, Dataset) of
        {<<"type">>,<<"kvm">>} ->
            generate_spec(Package, Dataset, OwnerData);
        {<<"type">>,<<"zone">>} ->
            generate_spec(Package, Dataset, OwnerData)
    end.

-spec to_sniffle(Spec::fifo:vm_config()) -> fifo:config_list().

to_sniffle(Spec) ->
    Spec1 = jsxd:from_list(Spec),
    case jsxd:get(<<"brand">>, <<"joyent">>, Spec1) of
        <<"kvm">> ->
            generate_sniffle(Spec1, kvm);
        <<"joyent">> ->
            generate_sniffle(Spec1, zone)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec generate_sniffle(Spec::fifo:vm_config(),
                       Type::fifo:vm_type()) -> fifo:config_list().

generate_sniffle(In, _Type) ->
    KeepKeys = [<<"state">>, <<"alias">>, <<"quota">>, <<"cpu_cap">>,
                <<"disk_driver">>, <<"vcpus">>, <<"nic_driver">>,
                <<"resolvers">>, <<"ram">>, <<"uuid">>, <<"cpu_shares">>],
    jsxd:fold(fun (<<"dataset_uuid">>, V, Obj) ->
                      jsxd:set(<<"dataset">>, V, Obj);
                  (<<"brand">>, <<"kvm">>, Obj) ->
                      jsxd:set(<<"type">>, <<"kvm">>, Obj);
                  (<<"brand">>, <<"joyent">>, Obj) ->
                      jsxd:set(<<"type">>, <<"zone">>, Obj);
                  (<<"max_physical_memory">>, V, Obj) ->
                      jsxd:update(<<"ram">>, fun(E) -> E end, round(V/(1024*1024)), Obj);
                  (<<"zonepath">>, V, Obj) ->
                      [_, Used, Avail | _] = re:split(os:cmd(binary_to_list(<<"/usr/sbin/zfs list -pH ", V/binary>>)), "\t", [{return, list}]),
                      {UsedI, _} = string:to_integer(Used),
                      {AvailI, _} = string:to_integer(Avail),
                      jsxd:set(<<"quota">>, round((UsedI + AvailI) / (1024*1024*1024)), Obj);
                  (<<"customer_metadata">>, V, Obj) ->
                      jsxd:fold(fun (<<"root_authorized_keys">>, V1, Obj1) ->
                                        jsxd:set(<<"ssh_keys">>, V1, Obj1);
                                    (<<"root_pw">>, V1, Obj1) ->
                                        jsxd:set(<<"root_pw">>, V1, Obj1);
                                    (<<"admin_pw">>, V1, Obj1) ->
                                        jsxd:set(<<"admin_pw">>, V1, Obj1);
                                    (K, V1, Obj1) ->
                                        jsxd:set([<<"metadata">>, K], V1, Obj1)
                                end, Obj, V);
                  (<<"disks">>, Disks, Obj) ->
                      {ObjOut, Size} = jsxd:fold(
                                         fun(_, Disk, {Obj1, Sum}) ->
                                                 Size = jsxd:get(<<"size">>, 0, Disk),
                                                 Total = Sum + round(Size / 1024),
                                                 case jsxd:get(<<"image_uuid">>, 1, Disk) of
                                                     undefined ->
                                                         {Obj1, Total};
                                                     Dataset ->
                                                         {jsxd:set(<<"dataset">>, Dataset, Obj1), Total}
                                                 end
                                         end, {Obj, 0}, Disks),
                      jsxd:set(<<"quota">>, Size, ObjOut);
                  (<<"nics">>, V, Obj) ->
                      jsxd:set(<<"networks">>, V, Obj);
                  (_,_,Obj) ->
                      Obj
              end, jsxd:select(KeepKeys, In), In).

-spec generate_spec(Package::fifo:package(),
                    Dataset::fifo:dataset(),
                    OwnerData::fifo:config()) -> fifo:vm_config().

generate_spec(Package, Dataset, OwnerData) ->
    {ok, Ram} = jsxd:get(<<"ram">>, Package),
    Base0 = jsxd:thread([{select, [<<"uuid">>, <<"alias">>]},
                         {set, <<"cpu_shares">>, Ram},
                         {merge, jsxd:select([<<"cpu_cap">>], Package)},
                         {merge, jsxd:select([<<"nic_driver">>,
                                              <<"disk_driver">>], Dataset)}],
                        OwnerData),
    Base1 = case jsxd:get(<<"type">>, Dataset) of
                {ok, <<"kvm">>} ->
                    Base01 = case jsxd:get(<<"cpu_cap">>, Base0) of
                                 {ok, V} ->
                                     jsxd:set(<<"vcpus">>, ceiling(V/100.0), Base0);
                                 _ ->
                                     Base0
                             end,
                    jsxd:thread([{set, <<"ram">>, Ram},
                                 {set, <<"brand">>, <<"kvm">>},
                                 {set, <<"max_physical_memory">>, Ram + 1024},
                                 {set, [<<"disks">>, 0, <<"boot">>], true},
                                 {set, [<<"disks">>, 0, <<"size">>],
                                  jsxd:get(<<"quota">>, 0, Package) * 1024},
                                 {set, [<<"disks">>, 0, <<"image_uuid">>],
                                  jsxd:get(<<"dataset">>, <<"">>, Dataset)}],
                                Base01);
                {ok, <<"zone">>} ->
                    jsxd:thread([{set, <<"max_physical_memory">>, Ram},
                                 {set, <<"brand">>, <<"joyent">>},
                                 {set, <<"quota">>,
                                  jsxd:get(<<"quota">>, 0, Package)},
                                 {set, <<"dataset_uuid">>,
                                  jsxd:get(<<"dataset">>, <<"">>, Dataset)}],
                                Base0)
            end,
    Base2 = jsxd:fold(fun (<<"ssh_keys">>, V, Obj) ->
                              jsxd:set([<<"customer_metadata">>, <<"root_authorized_keys">>], V, Obj);
                          (<<"root_pw">>, V, Obj) ->
                              jsxd:set([<<"customer_metadata">>, <<"root_pw">>], V, Obj);
                          (<<"resolvers">>, V, Obj) ->
                              jsxd:set(<<"resolvers">>, V, Obj);
                          (<<"admin_pw">>, V, Obj) ->
                              jsxd:set([<<"customer_metadata">>, <<"admin_pw">>], V, Obj);
                          (<<"metadata">>, V, Obj) ->
                              jsxd:update(<<"customer_metadata">>,
                                          fun(M) ->
                                                  jsxd:merge(M, V)
                                          end, V, Obj);
                          (_, _, Obj) ->
                              Obj
                      end, Base1, OwnerData),
    case jsxd:get(<<"networks">>, Dataset) of
        {ok, Nics} ->
            jsxd:thread([{set, <<"nics">>, Nics},
                         {set, [<<"nics">>, 0, <<"primary">>], true}],
                        Base2);
        _ ->
            Base2
    end.

-spec ceiling(X::float()) -> integer().

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

-ifdef(TEST).

type_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"alias">>, <<"vm">>}, {<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP},
                      {merge, InD},
                      {set, <<"cpu_shares">>,0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

disk_driver_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>},
                          {<<"disk_driver">>, <<"virtio">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_shares">>,0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

nic_driver_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>},
                          {<<"nic_driver">>, <<"virtio">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_shares">>,0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

zone_ram_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10},{<<"ram">>, 1024}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_shares">>,1024}],
                     InO),
    VMData = to_vmadm(InP, InD, InO),
    VMData1 = jsxd:set(<<"max_physical_memory">>, 1024*1024*1024, VMData),
    ?assertEqual(In, ordsets:from_list(to_sniffle(VMData1))).

kvm_ram_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 1024}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_shares">>,1024}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap1_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 100}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_shares">>,1024},
                      {set, <<"vcpus">>, 1}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap14_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 140}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_shares">>,1024},
                      {set, <<"vcpus">>, 2}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap15_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 150}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_shares">>,1024},
                      {set, <<"vcpus">>, 2}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap2_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 200}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_shares">>,1024},
                      {set, <<"vcpus">>, 2}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap21_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 210}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_shares">>,1024},
                      {set, <<"vcpus">>, 3}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).


resolver_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}, {<<"resolvers">>, [<<"8.8.8.8">>]}]),
    In = jsxd:thread([{merge, InP},
                      {merge, InD},
                      {set, <<"cpu_shares">>,0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

ssh_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>},
                          {<<"ssh_keys">>,
                           <<"ssh-rsa">>}]),
    In = jsxd:thread([{merge, InP},
                      {merge, InD},
                      {set, <<"cpu_shares">>,0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

passwd_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>},
                          {<<"admin_pw">>, <<"admin">>},
                          {<<"root_pw">>, <<"root">>}]),
    In = jsxd:thread([{merge, InP},
                      {merge, InD},
                      {set, <<"cpu_shares">>,0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

metadata_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>},
                          {<<"admin_pw">>, <<"admin">>},
                          {<<"metadata">>, [{<<"key">>, <<"value">>}]}]),
    In = jsxd:thread([{merge, InP},
                      {merge, InD},
                      {set, <<"cpu_shares">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

nics_test() ->
    InP = jsxd:from_list([{<<"quota">>, 10},{<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>},
                          {<<"networks">>, [[{<<"ip">>, <<"127.0.0.1">>},
                                             {<<"nic_tag">>, <<"admin">>}]]}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"zone uuid">>}]),
    In = jsxd:thread([{merge, InP},
                      {merge, InO}],
                     jsxd:from_list([{<<"cpu_shares">>, 0},
                                     {<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>},
                                     {<<"networks">>, [[{<<"ip">>, <<"127.0.0.1">>},
                                                        {<<"nic_tag">>, <<"admin">>},
                                                        {<<"primary">>, true}]]}])),

    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

-endif.
