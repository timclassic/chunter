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
	    generate_spec(Package, Dataset, OwnerData, kvm, unknown, [], [{<<"brand">>, <<"kvm">>}]);
	{<<"type">>,<<"zone">>} ->
	    generate_spec(Package, Dataset, OwnerData, zone, unknown, [], [{<<"brand">>, <<"joyent">>}])
    end.

-spec to_sniffle(Spec::fifo:vm_config()) -> fifo:config_list().

to_sniffle(Spec) ->
    case lists:keyfind(<<"brand">>, 1, Spec) of
	{<<"brand">>,<<"kvm">>} ->
	    generate_sniffle(Spec, [{<<"type">>, <<"kvm">>}], kvm);
	{<<"brand">>,<<"joyent">>} ->
	    generate_sniffle(Spec, [{<<"type">>, <<"zone">>}], zone)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec generate_sniffle(Spec::fifo:vm_config(),
		       Acc::fifo:config_list(),
		       Type::fifo:vm_type()) -> fifo:config_list().

generate_sniffle([{<<"state">>, V} | D], Acc, Type) ->
    generate_sniffle(D, [{<<"state">>, V} | Acc], Type);

generate_sniffle([{<<"dataset_uuid">>, V} | D], Acc, Type) ->
    generate_sniffle(D, [{<<"dataset">>, V} | Acc], Type);

generate_sniffle([{<<"quota">>, V} | D], Acc, zone = Type) ->
    generate_sniffle(D, [{<<"quota">>, V} | Acc], Type);

generate_sniffle([{<<"disks">>, Disks} | D], Acc, kvm = Type) ->
    case lists:foldl(fun(E, {Dataset, Sum}) ->
			     {<<"size">>, Size} = lists:keyfind(<<"size">>, 1, E),
			     Total = Sum + round(Size / 1024),
			     case lists:keyfind(<<"image_uuid">>, 1, E) of
				 {<<"image_uuid">>, NewDataset} ->
				     {NewDataset, Total};
				 _ ->
				     {Dataset, Total}
			     end
		     end, {undefined, 0}, Disks) of
	{undefined, Size} ->
	    generate_sniffle(D, [{<<"quota">>, Size} | Acc], Type);
	{Dataset, Size} ->
	    generate_sniffle(D, [{<<"quota">>, Size}, {<<"dataset">>, Dataset} | Acc], Type)
    end;

generate_sniffle([{<<"nics">>, Ns} | D], Acc, Type) ->
    generate_sniffle(D, [{<<"networks">>, Ns} | Acc], Type);

generate_sniffle([{<<"cpu_cap">>, V} | D], Acc, Type) ->
    generate_sniffle(D, [{<<"cpu_cap">>, V} | Acc], Type);

generate_sniffle([{<<"cpu_shares">>, V} | D], Acc, Type) ->
    generate_sniffle(D, [{<<"cpu_shares">>, V} | Acc], Type);

generate_sniffle([{<<"vcpus">>, V} | D], Acc, kvm = Type) ->
    generate_sniffle(D, [{<<"vcpus">>, V} | Acc], Type);

generate_sniffle([{<<"disk_driver">>, V} | D], Acc, kvm = Type) ->
    generate_sniffle(D, [{<<"disk_driver">>, V} | Acc], Type);

generate_sniffle([{<<"nic_driver">>, V} | D], Acc, kvm = Type) ->
    generate_sniffle(D, [{<<"nic_driver">>, V} | Acc], Type);

generate_sniffle([{<<"customer_metadata">>, V} | D], Acc, Type) ->
    generate_sniffle(D, decode_metadata(V, Acc, []), Type);

generate_sniffle([{<<"uuid">>, Ns} | D], Acc, Type) ->
    generate_sniffle(D, [{<<"uuid">>, Ns} | Acc], Type);

generate_sniffle([{<<"max_physical_memory">>, V} | D], Acc, zone = Type) ->
    generate_sniffle(D, [{<<"ram">>, round(V/1024/1024)} | Acc], Type);

generate_sniffle([{<<"ram">>, V} | D], Acc, kvm = Type) ->
    generate_sniffle(D, [{<<"ram">>, V} | Acc], Type);

generate_sniffle([{<<"resolvers">>, V} | D], Acc, Type) ->
    generate_sniffle(D, [{<<"resolvers">>, V} | Acc], Type);

generate_sniffle([_ | D], Acc, Type) ->
    generate_sniffle(D, Acc, Type);

generate_sniffle([], Acc, _Type) ->
    Acc.

-spec decode_metadata(Metadata::fifo:config_list(),
		      Acc::fifo:config_list(),
		      Other::fifo:config_list()) -> fifo:config_list().
decode_metadata([], Acc, []) ->
    Acc;

decode_metadata([], Acc, O) ->
    [{<<"metadata">>, O} | Acc];

decode_metadata([{<<"root_authorized_keys">>, Keys} | M], Acc, O) ->
    decode_metadata(M, [{<<"ssh_keys">>, Keys} | Acc], O);

decode_metadata([{<<"root_pw">>, Pass} | M], Acc, O) ->
    decode_metadata(M, [{<<"root_pw">>, Pass} | Acc], O);

decode_metadata([{<<"admin_pw">>, Pass} | M], Acc, O) ->
    decode_metadata(M, [{<<"admin_pw">>, Pass} | Acc], O);

decode_metadata([E | M], Acc, O) ->
    decode_metadata(M, Acc, [E | O]).


-spec generate_spec(Package::fifo:package(),
		    Dataset::fifo:dataset(),
		    OwnerData::fifo:config(),
		    Type::fifo:vm_type(),
		    DatasetUUID::fifo:uuid() | unknown,
		    Metadata::fifo:config_list(),
		    Acc::fifo:vm_config()) -> fifo:vm_config().


generate_spec([], [], [], _, _, Meta, Spec) ->
    [{<<"customer_metadata">>, Meta} | Spec];

generate_spec(P, D,  [{<<"uuid">>, V} | O], Type, DUUID, Meta, Spec) ->
    generate_spec(P, D, O, Type, DUUID, Meta, [{<<"uuid">>, V} | Spec]);

generate_spec(P, [{<<"dataset">>, DUUID} | D], O, T, _, Meta, Spec) ->
    generate_spec(P, D, O, T, DUUID, Meta, Spec);

generate_spec(P, [{<<"nic_driver">>, V} | D], O, T, DUUID, Meta, Spec) ->
    generate_spec(P, D, O, T, DUUID, Meta, [{<<"nic_driver">>, V} | Spec]);

generate_spec(P, [{<<"disk_driver">>, V} | D], O, T, DUUID, Meta, Spec) ->
    generate_spec(P, D, O, T, DUUID, Meta, [{<<"disk_driver">>, V} | Spec]);

generate_spec(P, [{<<"networks">>, N} | D], O, T, DUUID, Meta, Spec) ->
    generate_spec(P, D, O, T, DUUID, Meta, [{<<"nics">>, generate_nics(N, [])} | Spec]);

generate_spec([{<<"ram">>, V} | P], [], O, kvm = T, DUUID, Meta, Spec) ->
    generate_spec(P, [], O, T, DUUID, Meta, [{<<"cpu_shares">>, V},
					     {<<"ram">>, V},
					     {<<"max_physical_memory">>, V + 1024} | Spec]);

generate_spec([{<<"ram">>, V} | P], [], O, zone = T, DUUID, Meta, Spec) ->
    generate_spec(P, [], O, T, DUUID, Meta, [{<<"cpu_shares">>, V},
					     {<<"max_physical_memory">>, V} | Spec]);

generate_spec([{<<"cpu_cap">>, V} | P], [], O, kvm = T, DUUID, Meta, Spec) ->
    generate_spec(P, [], O, T, DUUID, Meta, [{<<"vcpus">>, ceiling(V/100.0)},
					     {<<"cpu_cap">>, V} | Spec]);

generate_spec([{<<"cpu_cap">>, V} | P], [], O, zone = T, DUUID, Meta, Spec) ->
    generate_spec(P, [], O, T, DUUID, Meta, [{<<"cpu_cap">>, V} | Spec]);


generate_spec([{<<"quota">>, V} | P], [], O, kvm = T, DUUID, Meta, Spec) ->
    generate_spec(P, [], O, T, DUUID, Meta,
		  [{<<"disks">>,
		    [[{<<"boot">>, true},
		     {<<"size">>, V * 1024},
		     {<<"image_uuid">>, DUUID}
		    ]]} | Spec]);

generate_spec([{<<"quota">>, V} | P], [], O, zone = T, DUUID, Meta, Spec) ->
    generate_spec(P, [], O, T, DUUID, Meta, [{<<"quota">>, V}, {<<"dataset_uuid">>, DUUID} | Spec]);

generate_spec([], [], [{<<"ssh_keys">>, V} | O], T, DUUID, Meta, Spec) ->
    generate_spec([], [], O, T, DUUID, [{<<"root_authorized_keys">>, V} | Meta], Spec);

generate_spec([], [], [{<<"root_pw">>, V} | O], T, DUUID, Meta, Spec) ->
    generate_spec([], [], O, T, DUUID, [{<<"root_pw">>, V} | Meta], Spec);

generate_spec([], [], [{<<"admin_pw">>, V} | O], T, DUUID, Meta, Spec) ->
    generate_spec([], [], O, T, DUUID, [{<<"admin_pw">>, V} | Meta], Spec);

generate_spec([], [], [{<<"metadata">>, V} | O], T, DUUID, Meta, Spec) ->
    generate_spec([], [], O, T, DUUID,  V ++ Meta, Spec);

generate_spec([], [], [{<<"resolvers">>, V} | O], T, DUUID, Meta, Spec) ->
    generate_spec([], [], O, T, DUUID, Meta,
		  [{<<"resolvers">>, V} | Spec]);

generate_spec([], [], [_ | O], Type, DUUID, Meta, Spec) ->
    generate_spec([], [], O, Type, DUUID, Meta, Spec);

generate_spec([_ | P], [], O, Type, DUUID, Meta, Spec) ->
    generate_spec(P, [], O, Type, DUUID, Meta, Spec);

generate_spec(P, [_ | D], O, Type, DUUID, Meta, Spec) ->
    generate_spec(P, D, O, Type, DUUID, Meta, Spec).


-spec generate_nics(Nics::[fifo:config_list()],
		    Acc::[fifo:config_list()]) -> [fifo:config_list()].

generate_nics([N | R], []) ->
    generate_nics(R, [[{<<"primary">>, true}| N]]);

generate_nics([N | R], Nics) ->
    generate_nics(R, [N | Nics]);

generate_nics([], Nics) ->
    Nics.

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
    InP = [{<<"quota">>, 10}],
    InD = [{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list(InP ++ InD ++ InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

disk_driver_test() ->
    InP = [{<<"quota">>, 10}],
    InD = [{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>},
	   {<<"disk_driver">>, <<"virtio">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list(InP ++ InD ++ InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

nic_driver_test() ->
    InP = [{<<"quota">>, 10}],
    InD = [{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>},
	   {<<"nic_driver">>, <<"virtio">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list(InP ++ InD ++ InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

zone_ram_test() ->
    InP = [{<<"quota">>, 10},{<<"ram">>, 1024}],
    InD = [{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list([{<<"cpu_shares">>, 1024} | InP ++ InD ++ InO]),
    VMData = to_vmadm(InP, InD, InO),
    VMData1 = lists:keydelete(<<"max_physical_memory">>, 1, VMData),
    VMData2 = [{<<"max_physical_memory">>, 1024*1024*1024} | VMData1],
    ?assertEqual(In, ordsets:from_list(to_sniffle(VMData2))).

kvm_ram_test() ->
    InP = [{<<"quota">>, 10}, {<<"ram">>, 1024}],
    InD = [{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list([{<<"cpu_shares">>, 1024} | InP ++ InD ++ InO]),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap1_test() ->
    InP = [{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 100}],
    InD = [{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list([{<<"vcpus">>, 1}, {<<"cpu_shares">>, 1024} | InP ++ InD ++ InO]),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap14_test() ->
    InP = [{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 140}],
    InD = [{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list([{<<"vcpus">>, 2}, {<<"cpu_shares">>, 1024} | InP ++ InD ++ InO]),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap15_test() ->
    InP = [{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 150}],
    InD = [{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list([{<<"vcpus">>, 2}, {<<"cpu_shares">>, 1024} | InP ++ InD ++ InO]),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap2_test() ->
    InP = [{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 200}],
    InD = [{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list([{<<"vcpus">>, 2}, {<<"cpu_shares">>, 1024} | InP ++ InD ++ InO]),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

kvm_cpu_cap21_test() ->
    InP = [{<<"quota">>, 10}, {<<"ram">>, 1024}, {<<"cpu_cap">>, 210}],
    InD = [{<<"type">>, <<"kvm">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list([{<<"vcpus">>, 3}, {<<"cpu_shares">>, 1024} | InP ++ InD ++ InO]),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).


resolver_test() ->
    InP = [{<<"quota">>, 10}],
    InD = [{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>}, {<<"resolvers">>, [<<"8.8.8.8">>]}],
    In = ordsets:from_list(InP ++ InD ++ InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

ssh_test() ->
    InP = [{<<"quota">>, 10}],
    InD = [{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>},
	  {<<"ssh_keys">>,
	   <<"ssh-rsa">>}],
    In = ordsets:from_list(InP ++ InD ++ InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

passwd_test() ->
    InP = [{<<"quota">>, 10}],
    InD = [{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>},
	  {<<"admin_pw">>, <<"admin">>},
	  {<<"root_pw">>, <<"root">>}],
    In = ordsets:from_list(InP ++ InD ++ InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

metadata_test() ->
    InP = [{<<"quota">>, 10}],
    InD = [{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>}],
    InO = [{<<"uuid">>, <<"zone uuid">>},
	   {<<"admin_pw">>, <<"admin">>},
	   {<<"metadata">>, [{<<"key">>, <<"value">>}]}],
    In = ordsets:from_list(InP ++ InD ++ InO),
    ?assertEqual(In, ordsets:from_list(to_sniffle(to_vmadm(InP, InD, InO)))).

nics_test() ->
    InP = [{<<"quota">>, 10},{<<"ram">>, 1024}],
    InD = [{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>},
	   {<<"networks">>, [[{<<"nic_tag">>, <<"admin">>},
			      {<<"ip">>, <<"127.0.0.1">>}]]}],
    InO = [{<<"uuid">>, <<"zone uuid">>}],
    In = ordsets:from_list(InP ++
			       [{<<"cpu_shares">>, 1024},
				{<<"type">>, <<"zone">>}, {<<"dataset">>, <<"datasetuuid">>},
				{<<"networks">>, [[{<<"primary">>, true},
						   {<<"nic_tag">>, <<"admin">>},
						   {<<"ip">>, <<"127.0.0.1">>}]]}]
			   ++ InO),
    VMData = to_vmadm(InP, InD, InO),
    VMData1 = lists:keydelete(<<"max_physical_memory">>, 1, VMData),
    VMData2 = [{<<"max_physical_memory">>, 1024*1024*1024} | VMData1],
    ?assertEqual(In, ordsets:from_list(to_sniffle(VMData2))).

-endif.
