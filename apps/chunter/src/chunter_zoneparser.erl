%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_zoneparser).

%% API
-export([convert/2, load/1]).


-define(REMOVE(Key),
	create_zone_data([{Key, _}|R], Disks, Nics, Datasets) ->
	       create_zone_data(R, Disks, Nics, Datasets)).

-define(RENAME_INT(Old,New),
	create_zone_data([{Old, Value}|R], Disks, Nics, Datasets) ->
	       {Num, []} = string:to_integer(binary_to_list(Value)),
	       [{New, Num}
		|create_zone_data(R, Disks, Nics, Datasets)]).

-define(RENAME_B64(Old,New),
	create_zone_data([{Old, Value}|R], Disks, Nics, Datasets) ->
	       [{New, base64:decode(Value)}
		|create_zone_data(R, Disks, Nics, Datasets)]).

-define(RENAME_BOOL(Old,New),
	create_zone_data([{Old, <<"true">>}|R], Disks, Nics, Datasets) ->
	       [{New, true}|create_zone_data(R, Disks, Nics, Datasets)];
	create_zone_data([{Old, <<"false">>}|R], Disks, Nics, Datasets) ->
	       [{New, false}|create_zone_data(R, Disks, Nics, Datasets)]).

-define(RENAME_SPLIT(Old,New),
	create_zone_data([{Old, Value}|R], Disks, Nics, Datasets) ->
	       [{New, re:split(Value,",")}
		|create_zone_data(R, Disks, Nics, Datasets)]).

-define(RENAME(Old,New),
	create_zone_data([{Old, Value}|R], Disks, Nics, Datasets) ->
	       [{New, Value}
		|create_zone_data(R, Disks, Nics, Datasets)]).

-define(NIC_RENAME(Old,New),
	create_nic([{Old, Value}|R]) ->
	       [{New, Value}
		|create_nic(R)]).
-define(NIC_RENAME_INT(Old,New),
	create_nic([{Old, Value}|R]) ->
	       {Num, []} = string:to_integer(binary_to_list(Value)),
	       [{New, Num}
		|create_nic(R)]).
-define(NIC_RENAME_BOOL(Old,New),
	create_nic([{Old, <<"true">>}|R]) ->
	       [{New, true}
		|create_nic(R)];
	create_nic([{Old, <<"false">>}|R]) ->
	       [{New, false}
		|create_nic(R)]).

-define(DISK_RENAME(Old,New),
	create_disk([{Old, Value}|R]) ->
	       [{New, Value}
		|create_disk(R)]).

-define(DISK_RENAME_INT(Old,New),
	create_disk([{Old, Value}|R]) ->
	       {Num, []} = string:to_integer(binary_to_list(Value)),
	       [{New, Num}
		|create_disk(R)]).
-define(DISK_RENAME_BOOL(Old,New),
	create_disk([{Old, <<"true">>}|R]) ->
	       [{New, true}
		|create_disk(R)];
	create_disk([{Old, <<"false">>}|R]) ->
	       [{New, false}
		|create_disk(R)]).
	       
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

load(VM) ->
    Name = proplists:get_value(<<"name">>, VM),
    convert(<<"/etc/zones/", Name/binary, ".xml">>, VM).
    
convert(F, VM)->
    {ok, XML} = file:read_file(F),
    case erlsom:simple_form(XML) of
        {ok,{"zone",Attrs,Value},_}->
            create_zone_data(VM ++ map_attrs(Attrs)  ++ parse_xml(Value));
        Err->
            Err
    end.

parse_xml([{"zone",Attrib,Value}|T])->
    [{"zone", Attrib, lists:flatten(parse_xml(Value))}|parse_xml(T)];

parse_xml([{"dataset",Attrib,_Value}|T])->
    [{dataset, list_to_binary(proplists:get_value("name", Attrib))}
     |parse_xml(T)];

parse_xml([{"attr",Attrib,_Value}|T])->
    [{list_to_binary(proplists:get_value("name", Attrib)), 
       list_to_binary(proplists:get_value("value", Attrib))}
     |parse_xml(T)];

parse_xml([{"rctl",Attrib,[{"rctl-value",
			   Values,
			   _}]}|T])->
    [{list_to_binary(proplists:get_value("name", Attrib)),
       list_to_binary(proplists:get_value("limit", Values))}
     |parse_xml(T)];

parse_xml([{"net-attr",[{"name",Name},{"value",Value}],[]}|T])->
    [{list_to_binary(Name), list_to_binary(Value)}|parse_xml(T)];
parse_xml([{"net-attr",[{"value",Value},{"name",Name}],[]}|T])->
    [{list_to_binary(Name), list_to_binary(Value)}|parse_xml(T)];

parse_xml([{"device",
	    [{"match",
	      Path}],
	    Content }|T]) ->
	  [{disk,
	     [{path, Path}|parse_xml(Content)]}
	   |parse_xml(T)];

parse_xml([{"network",
	    Attrs,
	    Content}|T]) ->
	  [{nic,
	    map_attrs(Attrs) ++ parse_xml(Content)}
	   |parse_xml(T)];

parse_xml([{Node,Attrib,Value}|T])->
    [{Node,Attrib, lists:flatten(parse_xml(Value))}|parse_xml(T)];

parse_xml(Value)->
    Value.

map_attrs(Attrs) ->
    lists:map(fun ({K,V}) -> 
		      {list_to_binary(K), list_to_binary(V)} 
	      end, Attrs).


create_zone_data(Data) ->
    create_zone_data(Data, [], [],[]).

create_zone_data([], [], [],[]) ->
    [];

create_zone_data([], [], [], Datasets) ->
    [{datasets, Datasets}];

create_zone_data([], [], Nics, Datasets) ->
    [{nics, Nics}|create_zone_data([], [], [], Datasets)];
    
create_zone_data([], Disks, Nics, Datasets) ->
    [{disks, Disks}|create_zone_data([], [], Nics, Datasets)];

create_zone_data([{disk, Disk}|R], Disks, Nics, Datasets) ->
   create_zone_data(R, [create_disk(Disk)|Disks], Nics, Datasets);

create_zone_data([{nic, Nic}|R], Disks, Nics, Datasets) ->
   create_zone_data(R, Disks, [create_nic(Nic)|Nics], Datasets);

create_zone_data([{dataset, Dataset}|R], Disks, Nics, Datasets) ->
   create_zone_data(R, Disks, Nics, [Dataset|Datasets]);

?REMOVE(<<"ip-type">>);
?REMOVE(<<"id">>);
?REMOVE(<<"debugid">>);
?RENAME(<<"uuid">>, <<"id">>);
?RENAME_B64(<<"alias">>, <<"alias">>);
?RENAME_BOOL(<<"autoboot">>, <<"autoboot">>);
?RENAME(<<"billing-id">>, <<"billing_id">>);
?RENAME_INT(<<"cpu-cap">>, <<"cpu-cap">>);
?RENAME_BOOL(<<"do-not-inventory">>, <<"do-not-inventory">>);
?RENAME(<<"name">>, <<"zonename">>);
?RENAME_BOOL(<<"never-booted">>, <<"never-booted">>);
?RENAME_B64(<<"qemu-extra-opts">>, <<"qemu-extra-opts">>);
?RENAME_B64(<<"qemu-opts">>, <<"qemu-opts">>);
?RENAME_INT(<<"ram">>, <<"ram">>);
?RENAME_SPLIT(<<"resolvers">>, <<"resolvers">>);
?RENAME_B64(<<"spice-opts">>, <<"spice-opts">>);
?RENAME_B64(<<"spice-password">>, <<"spice-password">>);
?RENAME_INT(<<"spice-port">>, <<"spice-port">>);
?RENAME_INT(<<"tempfs">>, <<"tempfs">>);
?RENAME_INT(<<"vcpus">>, <<"vcpus">>);
?RENAME_INT(<<"virtio-txburst">>, <<"virtio-txburst">>);
?RENAME_INT(<<"virtio-txtimer">>, <<"virtio-txtimer">>);
?RENAME_BOOL(<<"vm-autoboot">>, <<"vm-autoboot">>);
?RENAME_B64(<<"vnc-password">>, <<"vnc-password">>);
?RENAME_INT(<<"vnc-port">>, <<"vnc-port">>);
?RENAME_INT(<<"zoneid">>, <<"zoneid">>);
?RENAME_INT(<<"zone.cpu-cap">>, <<"cpu-cap">>);
?RENAME_INT(<<"zone.cpu-shares">>, <<"cpu-shares">>);
?RENAME_INT(<<"zone.max-locked-memory">>, <<"max-locked-memory">>);
?RENAME_INT(<<"zone.max-lwps">>, <<"max-lwps">>);
?RENAME_INT(<<"zone.max-physical-memory">>, <<"max-physical-memory">>);
?RENAME_INT(<<"zone.max-swap">>, <<"max-swap">>);
?RENAME_INT(<<"zone.zfs-io-priority">>, <<"zfs-io-priority">>);
create_zone_data([Pair|R], Disks, Nics, Datasets) ->
    [Pair|create_zone_data(R, Disks, Nics, Datasets)].


?NIC_RENAME(<<"ip">>, <<"ip">>);
?NIC_RENAME(<<"mac-addr">>, <<"mac">>);
?NIC_RENAME(<<"physical">>, <<"interface">>);
?NIC_RENAME_INT(<<"vlan-id">>, <<"vlan-id">>);
?NIC_RENAME(<<"global-nic">>, <<"nic-tag">>);
?NIC_RENAME_BOOL(<<"dhcp_server">>, <<"dhcp_server">>);

?NIC_RENAME(<<"blocked-outgoing-ports">>, <<"blocked_outgoing_ports">>);
create_nic([]) ->
    [];
create_nic([T|R]) ->
    [T|create_nic(R)].


?DISK_RENAME(<<"match">>, <<"path">>);
?DISK_RENAME_BOOL(<<"boot">>, <<"bool">>);
?DISK_RENAME_INT(<<"image-size">>, <<"image-size">>);
?DISK_RENAME_INT(<<"size">>, <<"size">>);
?DISK_RENAME(<<"image-uuid">>, <<"image-uuid">>);
?DISK_RENAME(<<"image-name">>, <<"image-name">>);
create_disk([]) ->
    [];
create_disk([T|R]) ->
    [T|create_nic(R)].
    
%% todo:
    %% <<"filesystem">>, {
    %%     <<"special">>, <<"source">>,
    %%     <<"directory">>, <<"target">>,
    %%     <<"type">>, <<"type">>,
    %%     <<"raw">>, <<"raw">>
    %% },
