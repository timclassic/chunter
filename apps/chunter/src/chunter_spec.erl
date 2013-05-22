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
         to_sniffle/1,
         create_update/3
        ]).

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
                <<"zfs_io_priority">>, <<"disk_driver">>, <<"vcpus">>, <<"nic_driver">>,
                <<"hostname">>, <<"autoboot">>, <<"created_at">>, <<"dns_domain">>,
                <<"resolvers">>, <<"ram">>, <<"uuid">>, <<"cpu_shares">>],
    jsxd:fold(fun (<<"internal_metadata">>, Int, Obj) ->
                      jsxd:merge(Int, Obj);
                  (<<"dataset_uuid">>, V, Obj) ->
                      jsxd:set(<<"dataset">>, V, Obj);
                  (<<"image_uuid">>, V, Obj) ->
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
                      jsxd:thread([{set, <<"zonepath">>, V},
                                   {set, <<"quota">>, round((UsedI + AvailI) / (1024*1024*1024))}], Obj);
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
                                                 case jsxd:get(<<"image_uuid">>, Disk) of
                                                     {ok, Dataset} ->
                                                         {jsxd:set(<<"dataset">>, Dataset, Obj1), Total};
                                                     _ ->
                                                         {Obj1, Total}
                                                 end
                                         end, {Obj, 0}, Disks),
                      jsxd:thread([{set, <<"quota">>, Size},
                                   {set, <<"disks">>, Disks}], ObjOut);
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
    RamPerc = case string:to_integer(os:cmd("/usr/sbin/prtconf | grep Memor | awk '{print $3}'")) of
                  {TotalMem, _} when is_number(TotalMem),
                                     TotalMem =/= 0 ->
                      io:format("~p~n", [TotalMem]),
                      Ram/TotalMem;
                  _ ->
                      0
              end,
    RamShare = round(1024*RamPerc),
    Base0 = jsxd:thread([{select, [<<"uuid">>, <<"alias">>]},
                         {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                         {set, <<"cpu_shares">>, jsxd:get(<<"cpu_shares">>, RamShare, Package)},
                         {set, <<"zfs_io_priority">>, jsxd:get(<<"zfs_io_priority">>, RamShare, Package)},
                         {set, [<<"internal_metadata">>, <<"package">>],
                          jsxd:get(<<"uuid">>, <<"-">>, Package)},
                         {set, <<"cpu_cap">>, jsxd:get([<<"cpu_cap">>], 100, Package)},
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
                    Base02 = jsxd:thread([{set, <<"ram">>, Ram},
                                          {set, <<"brand">>, <<"kvm">>},
                                          {set, <<"max_physical_memory">>, Ram + 1024},
                                          {set, [<<"disks">>, 0, <<"boot">>], true},
                                          {set, [<<"disks">>, 0, <<"image_size">>],
                                           jsxd:get(<<"image_size">>, 0, Dataset)},
                                          {set, [<<"disks">>, 0, <<"image_uuid">>],
                                           jsxd:get(<<"dataset">>, <<"">>, Dataset)}],
                                         Base01),
                    case jsxd:get(<<"quota">>, 0, Package) of
                        0 ->
                            Base02;
                        Q ->
                            jsxd:thread([{set, [<<"disks">>, 1, <<"boot">>], false},
                                         {set, [<<"disks">>, 1, <<"size">>],
                                          Q * 1024}],
                                        Base02)
                    end;
                {ok, <<"zone">>} ->
                    jsxd:thread([{set, <<"max_physical_memory">>, Ram},
                                 {set, <<"brand">>, <<"joyent">>},
                                 {set, <<"quota">>,
                                  jsxd:get(<<"quota">>, 0, Package)},
                                 {set, <<"image_uuid">>,
                                  jsxd:get(<<"dataset">>, <<"">>, Dataset)}],
                                Base0)
            end,
    Base2 = jsxd:fold(fun (<<"ssh_keys">>, V, Obj) ->
                              jsxd:set([<<"customer_metadata">>, <<"root_authorized_keys">>], V, Obj);
                          (<<"root_pw">>, V, Obj) ->
                              jsxd:set([<<"customer_metadata">>, <<"root_pw">>], V, Obj);
                          (<<"resolvers">>, V, Obj) ->
                              jsxd:set(<<"resolvers">>, V, Obj);
                          (<<"hostname">>, V, Obj) ->
                              jsxd:set(<<"hostname">>, V, Obj);
                          (<<"admin_pw">>, V, Obj) ->
                              jsxd:set([<<"customer_metadata">>, <<"admin_pw">>], V, Obj);
                          (<<"metadata">>, V, Obj) ->
                              jsxd:update(<<"customer_metadata">>,
                                          fun(M) ->
                                                  jsxd:merge(M, V)
                                          end, V, Obj);
                          (<<"note">>, V, Obj) ->
                              jsxd:set([<<"internal_metadata">>, <<"note">>], V, Obj);
                          (<<"network_map">>, V, Obj) ->
                              jsxd:set([<<"internal_metadata">>, <<"network_map">>], V, Obj);
                          (_, _, Obj) ->
                              Obj
                      end, Base1, OwnerData),
    Result = case jsxd:get(<<"networks">>, Dataset) of
                 {ok, Nics} ->
                     jsxd:thread([{set, <<"nics">>, Nics},
                                  {set, [<<"nics">>, 0, <<"primary">>], true}],
                                 Base2);
                 _ ->
                     Base2
             end,
    lager:debug("Converted ~p / ~p / ~p to: ~p.", [Package, Dataset, OwnerData, Result]),
    Result.


-spec create_update(Original::fifo:vm_config(),
                    Package::fifo:vm_config(),
                    Config::fifo:vm_config()) -> fifo:config_list().

create_update(_, [], Config) ->
    KeepKeys = [<<"resolvers">>, <<"hostname">>, <<"alias">>, <<"remove_nics">>, <<"add_nics">>,
                <<"update_nics">>],
    Result = jsxd:fold(fun (<<"ssh_keys">>, V, Obj) ->
                               jsxd:set([<<"set_customer_metadata">>, <<"root_authorized_keys">>], V, Obj);
                           (<<"root_pw">>, V, Obj) ->
                               jsxd:set([<<"set_customer_metadata">>, <<"root_pw">>], V, Obj);
                           (<<"admin_pw">>, V, Obj) ->
                               jsxd:set([<<"set_customer_metadata">>, <<"admin_pw">>], V, Obj);
                           (<<"metadata">>, V, Obj) ->
                               jsxd:update(<<"set_customer_metadata">>,
                                           fun(M) ->
                                                   jsxd:merge(M, V)
                                           end, V, Obj);
                           (<<"note">>, V, Obj) ->
                               jsxd:set([<<"set_internal_metadata">>, <<"note">>], V, Obj);
                           (_, _, Obj) ->
                               Obj
                       end,
                       jsxd:select(KeepKeys, Config),
                       Config),
    Result;

create_update(Original, Package, Config) ->
    Base = create_update(Original, [], Config),
    {ok, Ram} = jsxd:get(<<"ram">>, Package),
    RamPerc = case string:to_integer(os:cmd("/usr/sbin/prtconf | grep Memor | awk '{print $3}'")) of
                  {TotalMem, _} when is_number(TotalMem),
                                     TotalMem =/= 0 ->
                      io:format("~p~n", [TotalMem]),
                      Ram/TotalMem;
                  _ ->
                      0
              end,
    Base0 = jsxd:thread([{set, [<<"set_internal_metadata">>, <<"package">>],
                          jsxd:get(<<"uuid">>, <<"-">>, Package)},
                         {set, <<"cpu_shares">>, jsxd:get(<<"cpu_shares">>, round((1024*RamPerc)), Package)},
                         {set, <<"zfs_io_priority">>, jsxd:get(<<"zfs_io_priority">>, round((2048*RamPerc)), Package)},
                         {merge, jsxd:select([<<"cpu_cap">>], Package)}],
                        Base),
    Result = case jsxd:get(<<"brand">>, Original) of
                 {ok, <<"kvm">>} ->
                     Base01 = case jsxd:get(<<"cpu_cap">>, Base0) of
                                  {ok, V} ->
                                      jsxd:set(<<"vcpus">>, ceiling(V/100.0), Base0);
                                  _ ->
                                      Base0
                              end,
                     jsxd:thread([{set, <<"ram">>, Ram},
                                  {set, <<"max_physical_memory">>, Ram + 1024}],
                                 Base01);
                 {ok, <<"joyent">>} ->
                     jsxd:thread([{set, <<"max_physical_memory">>, Ram},
                                  {set, <<"quota">>,
                                   jsxd:get(<<"quota">>, 0, Package)}],
                                 Base0)
             end,
    lager:debug("Created Update package ~p / ~p / ~p to: ~p.", [Original, Package, Config, Result]),
    Result.

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
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"alias">>, <<"a">>}, {<<"hostname">>, <<"h">>}, {<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_cap">>, 100},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {delete, <<"name">>},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"cpu_shares">>,0},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    Res = ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO))),
    ?assertEqual(In, Res).

disk_driver_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"d">>},
                          {<<"disk_driver">>, <<"virtio">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_cap">>, 100},
                      {set, <<"vcpus">>, 1},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {delete, <<"name">>},
                      {set, <<"disks">>,
                       [[{<<"boot">>, true},
                         {<<"image_size">>, 0},
                         {<<"image_uuid">>, <<"d">>}],
                        [{<<"boot">>, false},
                         {<<"size">>, 10240}]]},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"cpu_shares">>,0},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    Res = ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO))),
%%%    ?debugFmt("~p~n~p", [In, Res]),
    ?assertEqual(In, Res).

created_at_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_cap">>, 100},
                      {set, <<"vcpus">>, 1},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {delete, <<"name">>},
                      {set, <<"disks">>,
                       [[{<<"boot">>, true},
                         {<<"image_size">>, 0},
                         {<<"image_uuid">>, <<"d">>}],
                        [{<<"boot">>, false},
                         {<<"size">>, 10240}]]},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"created_at">>, 123},
                      {set, <<"cpu_shares">>, 0},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    VMData = jsxd:set(<<"created_at">>,
                      123,
                      to_vmadm(InP, InD, InO)),
    ?assertEqual(In, ordsets:from_list(to_sniffle(VMData))).


nic_driver_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"d">>},
                          {<<"nic_driver">>, <<"virtio">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_cap">>, 100},
                      {set, <<"vcpus">>, 1},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {delete, <<"name">>},
                      {set, <<"disks">>,
                       [[{<<"boot">>, true},
                         {<<"image_size">>, 0},
                         {<<"image_uuid">>, <<"d">>}],
                        [{<<"boot">>, false},
                         {<<"size">>, 10240}]]},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"cpu_shares">>,0},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

zone_ram_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10},{<<"ram">>, 1024}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_cap">>, 100},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {delete, <<"name">>},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"cpu_shares">>, 0},
                      {set, <<"zfs_io_priority">>, 0},
                      {set, <<"uuid">>, <<"z">>}],
                     InO),
    VMData = to_vmadm(InP, InD, InO),
    VMData1 = jsxd:set(<<"max_physical_memory">>, 1024*1024*1024, VMData),
    ?assertEqual(In, ordsets:from_list(to_sniffle(VMData1))).

kvm_ram_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 1024}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {set, <<"cpu_cap">>, 100},
                      {set, <<"vcpus">>, 1},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {delete, <<"name">>},
                      {set, <<"disks">>,
                       [[{<<"boot">>, true},
                         {<<"image_size">>, 0},
                         {<<"image_uuid">>, <<"d">>}],
                        [{<<"boot">>, false},
                         {<<"size">>, 10240}]]},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"cpu_shares">>, 0},
                      {set, <<"zfs_io_priority">>, 0},
                      {set, <<"uuid">>, <<"z">>}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap1_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 100}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {delete, <<"name">>},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {set, <<"disks">>,
                       [[{<<"boot">>, true},
                         {<<"image_size">>, 0},
                         {<<"image_uuid">>, <<"d">>}],
                        [{<<"boot">>, false},
                         {<<"size">>, 10240}]]},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"cpu_shares">>, 0},
                      {set, <<"vcpus">>, 1},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap14_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 140}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {delete, <<"name">>},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {set, <<"disks">>,
                       [[{<<"boot">>, true},
                         {<<"image_size">>, 0},
                         {<<"image_uuid">>, <<"d">>}],
                        [{<<"boot">>, false},
                         {<<"size">>, 10240}]]},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"cpu_shares">>, 0},
                      {set, <<"vcpus">>, 2},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap15_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 150}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {delete, <<"name">>},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {set, <<"disks">>,
                       [[{<<"boot">>, true},
                         {<<"image_size">>, 0},
                         {<<"image_uuid">>, <<"d">>}],
                        [{<<"boot">>, false},
                         {<<"size">>, 10240}]]},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"cpu_shares">>, 0},
                      {set, <<"vcpus">>, 2},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap2_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 200}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {delete, <<"name">>},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {set, <<"disks">>,
                       [[{<<"boot">>, true},
                         {<<"image_size">>, 0},
                         {<<"image_uuid">>, <<"d">>}],
                        [{<<"boot">>, false},
                         {<<"size">>, 10240}]]},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"cpu_shares">>,0},
                      {set, <<"vcpus">>, 2},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap21_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 210}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP}, {merge, InD},
                      {delete, <<"name">>},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {set, <<"disks">>,
                       [[{<<"boot">>, true},
                         {<<"image_size">>, 0},
                         {<<"image_uuid">>, <<"d">>}],
                        [{<<"boot">>, false},
                         {<<"size">>, 10240}]]},
                      {set, <<"cpu_shares">>, 0},
                      {set, <<"vcpus">>, 3},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).


resolver_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}, {<<"resolvers">>, [<<"8.8.8.8">>]}]),
    In = jsxd:thread([{merge, InP},
                      {set, <<"cpu_cap">>, 100},
                      {delete, <<"name">>},
                      {set, <<"package">>, <<"p">>},
                      {merge, InD},
                      {set, <<"cpu_shares">>,0},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

ssh_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>},
                          {<<"ssh_keys">>,
                           <<"ssh-rsa">>}]),
    In = jsxd:thread([{merge, InP},
                      {set, <<"cpu_cap">>, 100},
                      {delete, <<"name">>},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {merge, InD},
                      {set, <<"cpu_shares">>,0},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

passwd_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>},
                          {<<"admin_pw">>, <<"admin">>},
                          {<<"root_pw">>, <<"root">>}]),
    In = jsxd:thread([{merge, InP},
                      {set, <<"cpu_cap">>, 100},
                      {delete, <<"name">>},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {merge, InD},
                      {set, <<"cpu_shares">>,0},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

metadata_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>},
                          {<<"admin_pw">>, <<"admin">>},
                          {<<"metadata">>, [{<<"key">>, <<"value">>}]}]),
    In = jsxd:thread([{merge, InP},
                      {set, <<"cpu_cap">>, 100},
                      {delete, <<"name">>},
                      {set, <<"package">>, <<"p">>},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {merge, InD},
                      {set, <<"cpu_shares">>, 0},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

nics_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10},{<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"d">>},
                          {<<"networks">>, [[{<<"ip">>, <<"127.0.0.1">>},
                                             {<<"nic_tag">>, <<"admin">>}]]}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = jsxd:thread([{merge, InP},
                      {set, <<"cpu_cap">>, 100},
                      {delete, <<"name">>},
                      {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                      {set, <<"package">>, <<"p">>},
                      {merge, InO},
                      {set, <<"uuid">>, <<"z">>},
                      {set, <<"zfs_io_priority">>, 0}],
                     jsxd:from_list([{<<"cpu_shares">>, 0},
                                     {<<"type">>, <<"zone">>}, {<<"dataset">>, <<"d">>},
                                     {<<"networks">>, [[{<<"ip">>, <<"127.0.0.1">>},
                                                        {<<"nic_tag">>, <<"admin">>},
                                                        {<<"primary">>, true}]]}])),

    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

-endif.
