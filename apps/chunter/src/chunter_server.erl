%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_server).

-behaviour(gen_server).

%% API
-export([start_link/0, list/0, get/1, get_vm/1, get_vm_pid/1, niceify_json/1,
	 set_total_mem/1,
	 set_provisioned_mem/1,
	 provision_memory/1,
	 unprovision_memory/1,
	 connect/0,
	 disconnect/0,
	 create_vm/8]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(CPU_CAP_MULTIPLYER, 8).

-define(SERVER, ?MODULE). 

-record(state, {name, 
		port, 
		connected = false,
		datasets = [],
		total_memory = 0, 
		provisioned_memory = 0}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get(UUID) ->
    gen_server:call(?SERVER, {call, system, {machines, get, UUID}}).

set_total_mem(M) ->
    gen_server:cast(?SERVER, {set_total_mem, M}).

set_provisioned_mem(M) ->
    gen_server:cast(?SERVER, {set_provisioned_mem, M}).

provision_memory(M) ->
    gen_server:cast(?SERVER, {prov_mem, M}).

unprovision_memory(M) ->
    gen_server:cast(?SERVER, {unprov_mem, M}).

connect() ->
    gen_server:cast(?SERVER, connect).

disconnect() ->
    gen_server:cast(?SERVER, disconnect).

list() ->
    gen_server:call(?SERVER, {call, system, {machines, list}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    lager:info([{fifi_component, chunter}],
	       "chunter:init.", []),
    % We subscribe to sniffle register channel - that way we can reregister to dead sniffle processes.
    [Host|_] = re:split(os:cmd("uname -n"), "\n"),
    zmq_mdns_connection_event:add_handler(chunter_connect_event),
    libsniffle:hypervisor_register(Host, Host, 4200),
    lager:info([{fifi_component, chunter}],
	       "chunter:init - Host: ~s", [Host]),
    {ok, #state{name=Host}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({call, Auth, {machines, list}}, _From,  #state{name=Name} = State) ->
%    statsderl:increment([Name, ".call.machines.list"], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "machines:list.", []),
    VMS = list_vms(Auth, Name),
    {reply, {ok, VMS}, State};

handle_call({call, Auth, {machines, get, UUID}}, _From, #state{name=Name} =  State) ->
%    statsderl:increment([Name, ".call.machines.get.", UUID], 1, 1.0),

    lager:info([{fifi_component, chunter}],
	       "machines:get - UUID: ~s.", [UUID]),
    case libsnarl:allowed(system, Auth, [host, Name, vm, UUID, get]) of
	false ->
	    lager:warning([{fifi_component, chunter}],
			  "machines:info - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    Pid = get_vm_pid(UUID),
	    {ok, Reply} = chunter_vm:get(Pid),
	    {reply, {ok, Reply}, State}
    end;

handle_call({call, Auth, {info, memory}}, _From, #state{name = Name,
							total_memory = T, 
							provisioned_memory = P} = State) ->
%    statsderl:increment([Name, ".call.info.memory"], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "hypervisor:info.memory.", []),
    case libsnarl:allowed(system, Auth, [host, Name, info, memory]) of
	false ->
	    lager:warning([{fifi_component, chunter}],
			  "hypervisor:info.memory - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    {reply, {ok, {P, T}}, State}
    end;

% TODO
handle_call({call, Auth, {machines, info, UUID}}, _From, #state{name = Name} = State) ->
%    statsderl:increment([Name, ".call.machines.info.", UUID], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "machines:info - UUID: ~s.", [UUID]),
    case libsnarl:allowed(system, Auth, [host, Name, vm, UUID, info]) of
	false ->
	    lager:warning([{fifi_component, chunter}],
			  "machines:info - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    Pid = get_vm_pid(UUID),
	    {ok, Reply} = chunter_vm:info(Pid),
	    {reply, {ok, Reply}, State}
    end;

handle_call({call, Auth, {packages, list}}, _From, #state{name = _Name} = State) ->
%    statsderl:increment([Name, ".call.packages.list"], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "packages:list", []),
    case libsnarl:allowed(system, Auth, [package, list]) of
	false ->
	    lager:warning([{fifi_component, chunter}],
			  "packages:list - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    Reply = [], 
	    {reply, {ok,  Reply}, State}
    end;

handle_call({call, Auth, {datasets, list}}, _From, #state{datasets=Ds, name=_Name} = State) ->
%    statsderl:increment([Name, ".call.datasets.list"], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "datasets:list", []),
    {Reply, Ds1} = list_datasets(Ds, Auth), 
    {reply, {ok, Reply}, State#state{datasets=Ds1}};

handle_call({call, Auth, {datasets, get, UUID}}, _From, #state{datasets=Ds, name=_Name} = State) ->
%    statsderl:increment([Name, ".call.datasets.get.", UUID], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "datasets:get - UUID: ~s.", [UUID]),
    case libsnarl:allowed(system, Auth, [dataset, UUID, get]) of
	false ->
	    lager:warning([{fifi_component, chunter}],
			  "datasets:get - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    {Reply, Ds1} = get_dataset(UUID, Ds), 
	    {reply, {ok, Reply}, State#state{datasets=Ds1}}
    end;


handle_call({call, Auth, {keys, list}}, _From, #state{name=Name} = State) ->
%    statsderl:increment([Name, ".call.keys.list"], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "keys:list", []),
    case libsnarl:allowed(system, Auth, [host, Name, key, list]) of
	false ->
	    lager:warning([{fifi_component, chunter}],
			  "keys:list - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    Reply = [], 
	    {reply, {ok, Reply}, State}
    end;


handle_call({call, _Auth, Call}, _From, #state{name = _Name} = State) ->
%    statsderl:increment([Name, ".call.unknown"], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "unsupported call - ~p", [Call]),
    Reply = {error, {unsupported, Call}},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknwon}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(connect,  #state{name = Host} = State) ->
%    {ok, Host} = libsnarl:option_get(system, statsd, hostname),
%    application:set_env(statsderl, hostname, Host),
%    application:start(statsderl),
    {TotalMem, _} = string:to_integer(os:cmd("/usr/sbin/prtconf | grep Memor | awk '{print $3}'")),
    VMS = list_vms(system, Host),
    ProvMem = round(lists:foldl(fun (VM, Mem) ->
				  {max_physical_memory, M} = lists:keyfind(max_physical_memory, 1, VM),
				  Mem + M
			  end, 0, VMS) / (1024*1024)),
%    statsderl:gauge([Name, ".hypervisor.memory.total"], TotalMem, 1),
%    statsderl:gauge([Name, ".hypervisor.memory.provisioned"], ProvMem, 1),
    
%    statsderl:increment([Name, ".net.join"], 1, 1.0),
%    libsniffle:join_client_channel(),
    libsniffle:hypervisor_register(Host, Host, 4200),

    {noreply, State#state{
		total_memory = TotalMem,
		provisioned_memory = ProvMem,
		connected = true
	       }};

handle_cast(disconnect,  State) ->
    {noreply, State#state{connected = false}};

handle_cast({cast, Auth, {machines, create, VMName, PackageUUID, DatasetUUID, Metadata, Tags}},
	    #state{datasets=Ds, name=Name} = State) ->
    spawn(chunter_server, create_vm, [Auth, VMName, PackageUUID, DatasetUUID, Metadata, Tags, Ds, Name]),
    {noreply, State};

handle_cast({set_total_mem, M}, State = #state{name = _Name}) ->
%    statsderl:gauge([Name, ".hypervisor.memory.total"], M, 1),
    {noreply, State#state{total_memory= M}};


handle_cast({set_provisioned_mem, M}, State = #state{name = _Name,
						     provisioned_memory = P,
						     total_memory = T}) ->
    MinMB = round(M / (1024*1024)),
    Diff = round(P - MinMB),
%    statsderl:gauge([Name, ".hypervisor.memory.provisioned"], MinMB, 1),
    lager:info([{fifi_component, chunter}],
	       "memory:provision - Privisioned: ~p(~p), Total: ~p, Change: ~p .", [MinMB, M, T, Diff]),    
    {noreply, State#state{provisioned_memory = MinMB}};


handle_cast({prov_mem, M}, State = #state{name = _Name,
					  provisioned_memory = P,
					  total_memory = T}) ->
    MinMB = round(M / (1024*1024)),
    Res = round(MinMB + P),
%    statsderl:gauge([Name, ".hypervisor.memory.provisioned"], Res, 1),
    lager:info([{fifi_component, chunter}],
	       "memory:provision - Privisioned: ~p(~p), Total: ~p, Change: +~p.", [Res, M, T, MinMB]),    
    {noreply, State#state{provisioned_memory = Res}};

handle_cast({unprov_mem, M}, State = #state{name = _Name,
					    provisioned_memory = P, 
					    total_memory = T}) ->
    MinMB = round(M / (1024*1024)),
    Res = round(P - MinMB),
%    statsderl:gauge([Name, ".hypervisor.memory.provisioned"], Res, 1),
    lager:info([{fifi_component, chunter}],
	       "memory:unprovision - Unprivisioned: ~p(~p) , Total: ~p, Change: -~p.", [Res, M, T, MinMB]),
    {noreply, State#state{provisioned_memory = Res}};

handle_cast({cast, Auth, {machines, start, UUID}}, #state{name = Name} = State) ->
%    statsderl:increment([Name, ".cast.machines.start.", UUID], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "machines:start - UUID: ~s.", [UUID]),
    case libsnarl:allowed(system, Auth, [host, Name, vm, UUID, start]) of
	false ->
	    lager:warning([{fifi_component, chunter}],
			  "machines:start - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    spawn(chunter_vmadm, start, [UUID]),
	    {noreply, State}
    end;

handle_cast({cast, Auth, {machines, delete, UUID}}, #state{name = Name} = State) ->
%    statsderl:increment([Name, ".cast.machines.delete"], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "machines:delete - UUID: ~s.", [UUID]),
    case libsnarl:allowed(system, Auth, [host, Name, vm, UUID, delete]) of
	false ->
	    lager:warning([{fifi_component, chunter}],
			  "machines:delete - forbidden Auth: ~p.", [Auth]),
%	    libsnarl:msg(Auth, error, <<"You don't have the permissions to delete the VM '", UUID/binary,"'.">>),
	    {reply, {error, forbidden}, State};
	true ->
	    VM = get_vm(UUID),
	    case proplists:get_value(nics, VM) of
		undefined ->
		    [];
		Nics ->
		    [try
			 Net = proplists:get_value(nic_tag, Nic),
			 IP = proplists:get_value(ip, Nic),
			 libsnarl:network_release_ip(system, Net, IP),
			 ok
		     catch 
			 _:_ ->
			     ok
		     end
		     || Nic <- Nics]
	    end,
	    case libsnarl:group_get(system, <<"vm_", UUID/binary, "_owner">>) of
		{ok, GUUID} ->
		    libsnarl:group_delete(system, GUUID);
		_ -> 
		    ok
	    end,
	    {max_physical_memory, Mem} = lists:keyfind(max_physical_memory, 1, VM),
%	    libsnarl:msg(Auth, success, <<"VM '", UUID/binary,"' is being deleted.">>),
	    spawn(chunter_vmadm, delete, [UUID, Mem]),
	    {noreply, State}
    end;


handle_cast({cast, Auth, {machines, start, UUID, Image}}, #state{name = Name} =State) ->
%    statsderl:increment([Name, ".cast.machines.start_image.", UUID], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "machines:start - UUID: ~s, Image: ~s.", [UUID, Image]),

    case libsnarl:allowed(system, Auth, [host, Name, vm, UUID, start]) of
	false ->
%	    libsnarl:msg(Auth, error, <<"You don't have the permissions to start the VM '", UUID/binary,"'.">>),
	    lager:warning([{fifi_component, chunter}],
			  "machines:start - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    spawn(chunter_vmadm, start, [UUID, Image]),
	    {noreply, State}
    end;


handle_cast({cast, Auth, {machines, stop, UUID}}, #state{name = Name} = State) ->
%    statsderl:increment([Name, ".cast.machines.stop.", UUID], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "machines:stop - UUID: ~s.", [UUID]),
    case libsnarl:allowed(system, Auth, [host, Name, vm, UUID, stop]) of
	false ->
%	    libsnarl:msg(Auth, error, <<"You don't have the permissions to stop the VM '", UUID/binary,"'.">>),
	    lager:warning([{fifi_component, chunter}],
			  "machines:stop - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    spawn(chunter_vmadm, stop, [UUID]),
	    {noreply, State}
    end;

handle_cast({cast, Auth, {machines, reboot, UUID}}, #state{name = Name} =State) ->
%    statsderl:increment([Name, ".cast.machines.reboot.", UUID], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "machines:reboot - UUID: ~s.", [UUID]),
    case libsnarl:allowed(system, Auth, [host, Name, vm, UUID, reboot]) of
	false ->
%	    libsnarl:msg(Auth, error, <<"You don't have the permissions to reboot the VM '", UUID/binary,"'.">>),
	    lager:warning([{fifi_component, chunter}],
			  "machines:reboot - forbidden Auth: ~p.", [Auth]),
	    {reply, {error, forbidden}, State};
	true ->
	    spawn(chunter_vmadm, reboot, [UUID]),
	    {noreply, State}
    end;


handle_cast(Msg, #state{name = _Name} = State) ->
    lager:warning("Unknwn cast: ~p", Msg),
%    statsderl:increment([Name, ".cast.unknown"], 1, 1.0),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


get_vm(ZUUID) ->
    [Hypervisor|_] = re:split(os:cmd("uname -n"), "\n"),
    [VM] = [chunter_zoneparser:load([{hypervisor, Hypervisor}, {name,Name},{state, VMState},{zonepath, Path},{uuid, UUID},{type, Type}]) || 
	       [ID,Name,VMState,Path,UUID,Type,_IP,_SomeNumber] <- 
		   [ re:split(Line, ":") 
		     || Line <- re:split(os:cmd("/usr/sbin/zoneadm -u" ++ binary_to_list(ZUUID) ++ " list -p"), "\n")],
	       ID =/= <<"0">>],
    VM.

list_vms(Auth, Hypervisor) ->
%    {ok, AuthC} = libsnarl:user_cache(system, Auth),
    [chunter_zoneparser:load([{hypervisor, Hypervisor}, {name,Name},{state, VMState},{zonepath, Path},{uuid, UUID},{type, Type}]) || 
	[ID,Name,VMState,Path,UUID,Type,_IP,_SomeNumber] <- 
	    [ re:split(Line, ":") 
	      || Line <- re:split(os:cmd("/usr/sbin/zoneadm list -ip"), "\n")],
	ID =/= <<"0">> 
%	    andalso libsnarl:allowed(system, AuthC, [host, Name, vm, UUID, get]) == true
    ].

get_dataset(UUID, Ds) ->
    read_dsmanifest(filename:join(<<"/var/db/imgadm">>, <<UUID/binary, ".json">>), Ds).

list_datasets(Datasets, Auth) ->
    {ok, AuthC} = libsnarl:user_cache(system, Auth),
    filelib:fold_files("/var/db/imgadm", ".*json", false, 
		       fun ("/var/db/imgadm/imgcache.json", R) ->
			       R;
			   (F, {Fs, DsA}) ->
			       {match, [UUID]} = re:run(F, "/var/db/imgadm/(.*)\.json", 
							[{capture, all_but_first, binary}]),
			       case libsnarl:allowed(system, AuthC, [dataset, UUID, get]) of
				   true ->
				       {F1, DsA1} = read_dsmanifest(F, DsA),
				       {[F1| Fs], DsA1};
				   false ->
				       {Fs, DsA}
			       end
		       end, {[], Datasets}).
			       
read_dsmanifest(F, Ds) ->
    case proplists:get_value(F, Ds) of
	undefined ->
	    {ok, Data} = file:read_file(F),
	    JSON = jsx:json_to_term(Data),
	    JSON1 = niceify_json(JSON),
	    ID = proplists:get_value(uuid, JSON1),
	    JSON2 = [{id, ID}|JSON1],
	    {JSON2, [{F, JSON2}|Ds]};
	JSON -> 
	    {JSON, Ds}
    end.

niceify_json([{K, V}|R]) when is_list(V), is_binary(K) ->
    [{binary_to_atom(K), niceify_json(V)}|niceify_json(R)];

niceify_json([{K, V}|R]) when is_list(V) ->
    [{K, niceify_json(V)}|niceify_json(R)];

niceify_json([{K, V}|R]) when is_binary(K) ->
    [{binary_to_atom(K), V}|niceify_json(R)];

niceify_json([H|R]) ->
    [H|niceify_json(R)];

niceify_json([]) ->
    [].

binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).
    

get_vm_pid(UUID) ->
    try gproc:lookup_pid({n, l, {vm, UUID}}) of
	Pid ->
	    Pid
    catch
	_T:_E ->
	    {ok, Pid} = chunter_vm_sup:start_child(UUID),
	    Pid
    end.

install_image(DatasetUUID) ->
    case filelib:is_regular(filename:join(<<"/var/db/imgadm">>, <<DatasetUUID/binary, ".json">>)) of
	true ->
	    ok;
	false ->
%	    libsnarl:msg(Auth, <<"warning">>, <<"Dataset needs to be imported!">>),
	    os:cmd(binary_to_list(<<"/usr/sbin/imgadm import ", DatasetUUID/binary>>))
    end.

create_vm(Auth, VMName, PackageUUID, DatasetUUID, Metadata, Tags, Ds, Name) ->
%    statsderl:increment([Name, ".call.machines.create"], 1, 1.0),
    lager:info([{fifi_component, chunter}],
	       "machines:create - Name: ~s, Package: ~s, Dataset: ~s.", [VMName, PackageUUID, DatasetUUID]),
    lager:debug([{fifi_component, chunter}],
		"machines:create - Meta Data: ~p, Tags: ~p.", [Metadata, Tags]),
    case libsnarl:allowed(system, Auth, [host, Name, vm, create]) of
	true ->
	    install_image(DatasetUUID),
	    {Dataset, _Ds} = get_dataset(DatasetUUID, Ds),
	    lager:debug([{fifi_component, chunter}],
			"machines:create - Dataset Data: ~p.", [Dataset]),
	    {ok, Package} = libsnarl:option_get(system, <<"packages">>, PackageUUID),
	    {Memory, []} = string:to_integer(binary_to_list(proplists:get_value(memory, Package))),
	    {Disk, []} = string:to_integer(binary_to_list(proplists:get_value(disk, Package))),
	    {Swap,[]} = string:to_integer(binary_to_list(proplists:get_value(swap, Package))),
	    lager:info([{fifi_component, chunter}],
		       "machines:create - Memroy: ~pMB, Disk: ~pGB, Swap: ~pMB.", [Memory, Disk, Swap]),
	    Reply = [{owner_uuid, Auth},
		     {tags, Tags},
		     {customer_metadata, Metadata},
		     {alias, VMName}],
	    DiskDrv = proplists:get_value(disk_driver, Dataset, <<"virtio">>),
	    NicDrv = proplists:get_value(nic_driver, Dataset, <<"virtio">>),
	    lager:info([{fifi_component, chunter}],
		       "machines:create - Disk driver: ~s, net driver: ~s.", 
		       [DiskDrv, NicDrv]),
	    
	    {Reply1, Rights} = case libsnarl:network_get_ip(Auth, <<"admin">>) of
				   {ok, IP} ->
				       {ok, {_, Mask, Gateway, _}} = libsnarl:network_get(Auth, <<"admin">>),
				       IPStr = libsnarl:ip_to_str(IP),
				       MaskStr = libsnarl:ip_to_str(Mask),
				       GWStr = libsnarl:ip_to_str(Gateway),
				       lager:info([{fifi_component, chunter}],
						  "machines:create -  ~s mask ~s gw ~s.", 
						  [IPStr, MaskStr, GWStr]),
				       {[{nics, 
					  [[
					    {nic_tag, <<"admin">>},
					    {ip, IPStr},
					    {netmask, MaskStr},
					    {gateway, GWStr},
					    {model, NicDrv}
					   ]]}|Reply],
					[[network, <<"admin">>, release, libsnarl:ip_to_str(IP)]]};
				   _ ->
				       lager:warning([{fifi_component, chunter}],
						     "create machines - could not obtain IP.", []),
				       {Reply, []}
			       end,
	    Reply2 = case proplists:get_value(os, Dataset) of
			 <<"smartos">> = OS ->
%			     statsderl:increment([Name, ".call.machines.create.smartos"], 1, 1.0),
			     lager:info([{fifi_component, chunter}],
					"machines:create - os type ~s.", 
					[OS]),
			     [{max_physical_memory, Memory},
			      {quota, Disk},
			      {cpu_share, Memory},
			      {cpu_cap, Memory * ?CPU_CAP_MULTIPLYER},
			      {max_swap, Swap},
			      {dataset_uuid, DatasetUUID}
			      |Reply1];
			 OS ->
%			     statsderl:increment([Name, ".call.machines.create.kvm"], 1, 1.0),
			     lager:info([{fifi_component, chunter}],
					"machines:create - os type ~s.", 
					[OS]),
			     Res = [{max_physical_memory, Memory+1024},
				    {ram, Memory},
				    {brand, <<"kvm">>},
				    {disks,
				     [[{size, Disk*1024},
				       {model, DiskDrv},
				       {image_uuid, DatasetUUID}]]},
				    {disk_driver, DiskDrv},
				    {nic_driver, NicDrv},
				    {max_swap, Swap}
				    |Reply1],
			     Res
		     end,
	    lager:debug([{fifi_component, chunter}],
			"machines:create - final spec: ~p.", 
			[Reply2]),
	    chunter_vmadm:create(Reply2, Auth, Rights),
	    ok;
	_ ->
	    lager:warning([{fifi_component, chunter}],
			  "machines:create - forbidden Auth: ~p.", [Auth]),
%	    libsnarl:msg(Auth, <<"error">>, <<"Could not create VM!">>),
	    ok
    end.
