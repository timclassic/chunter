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
-export([start_link/0, list/0, get/1, get_vm/1, get_vm_pid/1, niceify_json/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {port, datasets=[]}).

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

list() ->
    gen_server:call(?SERVER, {call, system, {machines, list}}).
reregister() ->
    gen_server:cast(?SERVER, reregister).

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
    % We subscribe to sniffle register channel - that way we can reregister to dead sniffle processes.
    {ok, #state{}, 1000}.


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
handle_call({call, Auth, {machines, list}}, _From, State) ->
    Reply = list_vms(Auth),
    {reply, {ok, Reply}, State};

handle_call({call, Auth, {machines, get, UUID}}, _From, State) ->
    case libsnarl:allowed(system, Auth, [vm, UUID, view]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    Pid = get_vm_pid(UUID),
	    {ok, Reply} = chunter_vm:get(Pid),
	    {reply, {ok, Reply}, State}
    end;

handle_call({call, Auth, {machines, create, Name, PackageUUID, DatasetUUID, Metadata, Tags}}, From, 
	    #state{datasets=Ds} = State) ->
    case libsnarl:allowed(system, Auth, [vm, create]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    {Dataset, Ds1} = get_dataset(DatasetUUID, Ds),
	    {ok, Package} = libsnarl:option_get(Auth, packages, PackageUUID),
	    Memory = proplists:get_value(memory, Package),
	    Disk = proplists:get_value(disk, Package),
	    Swap = proplists:get_value(swap, Package),
	    Reply = [{tags, Tags},
		     {customer_metadata, Metadata},
		     {alias, Name}],
	    
	    {Reply1, Rights} = case libsnarl:network_get_ip(Auth, <<"external">>) of
			       {ok, IP} ->
				       {ok, {_, Mask, Gateway, _}} = libsnarl:network_get(Auth, <<"external">>),
				       {[{nics, 
					  [[
					    {nic_tag, <<"external">>},
					    {ip, libsnarl:ip_to_str(IP)},
					    {netmask, libsnarl:ip_to_str(Mask)},
					    {gateway, libsnarl:ip_to_str(Gateway)}
					   ]]}|Reply],
					[network, <<"external">>, release, IP]};
				   _ ->
				       {undefiend, []}
			       end,
	    Reply2 = case proplists:get_value(platform_type, Dataset) of
			 <<"smartos">> ->
			     [{max_physical_memory, Memory},
			      {quota, Disk},
			      {max_swap, Swap},
			      {dataset_uuid, DatasetUUID}
			      |Reply1];
			 _ ->
			     [{max_physical_memory, Memory+1024},
			      {ram, Memory},
			      {quota, 10},
			      {disk_driver, proplists:get_value(disk_driver, Dataset)},
			      {nic_driver, proplists:get_value(nic_driver, Dataset)},
			      {max_swap, Swap},
			      {dataset_uuid, DatasetUUID}
			      |Reply1]
		     end,
	    spawn(chunter_vmadm, create, [Reply2, From, Auth, Rights]),
	    {noreply,  State#state{datasets=Ds1}}
    end;

% TODO
handle_call({call, Auth, {machines, info, UUID}}, _From, State) ->
    case libsnarl:allowed(system, Auth, [vm, UUID, info]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    Pid = get_vm_pid(UUID),
	    {ok, Reply} = chunter_vm:info(Pid),
	    {reply, {ok, Reply}, State}
    end;

handle_call({call, Auth, {packages, list}}, _From, State) ->
    case libsnarl:allowed(system, Auth, [package, list]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    Reply = [], 
	    {reply, {ok,  Reply}, State}
    end;

handle_call({call, Auth, {datasets, list}}, _From, #state{datasets=Ds} = State) ->
    {Reply, Ds1} = list_datasets(Ds, Auth), 
    {reply, {ok, Reply}, State#state{datasets=Ds1}};

handle_call({call, Auth, {datasets, get, UUID}}, _From, #state{datasets=Ds} = State) ->
    case libsnarl:allowed(system, Auth, [dataset, UUID, get]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    {Reply, Ds1} = get_dataset(UUID, Ds), 
	    {reply, {ok, Reply}, State#state{datasets=Ds1}}
    end;


handle_call({call, Auth, {keys, list}}, _From, State) ->
    case libsnarl:allowed(system, Auth, [key, list]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    Reply = [], 
	    {reply, {ok, Reply}, State}
    end;


handle_call({call, _Auth, Call}, _From, State) ->
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
handle_cast({cast, Auth, {machines, start, UUID}}, State) ->
    case libsnarl:allowed(system, Auth, [vm, UUID, start]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    spawn(chunter_vmadm, start, [UUID]),
	    {noreply, State}
    end;

handle_cast({cast, Auth, {machines, delete, UUID}}, State) ->
    case libsnarl:allowed(system, Auth, [vm, UUID, delete]) of
	false ->
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
	    spawn(chunter_vmadm, delete, [UUID]),
	    {noreply, State}
    end;


handle_cast({cast, Auth, {machines, start, UUID, Image}}, State) ->
    case libsnarl:allowed(system, Auth, [vm, UUID, start]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    spawn(chunter_vmadm, start, [UUID, Image]),
	    {noreply, State}
    end;


handle_cast({cast, Auth, {machines, stop, UUID}}, State) ->
    case libsnarl:allowed(system, Auth, [vm, UUID, stop]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    spawn(chunter_vmadm, stop, [UUID]),
	    {noreply, State}
    end;

handle_cast({cast, Auth, {machines, reboot, UUID}}, State) ->
    case libsnarl:allowed(system, Auth, [vm, UUID, reboot]) of
	false ->
	    {reply, {error, forbidden}, State};
	true ->
	    spawn(chunter_vmadm, reboot, [UUID]),
	    {noreply, State}
    end;

handle_cast(reregister, State) ->
    try
	libsniffle:join_client_channel(),
        libsniffle:register(system, chunter, self()),
	{noreply, State}
    catch
	_T:_E ->
	    application:stop(gproc),
	    application:start(gproc),
	    {noreply, State, 1000}
    end;


handle_cast(_Msg, State) ->
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
    reregister(),
    {noreply, State};

handle_info({sniffle, request, register}, State) ->
    libsniffle:register(system, chunter, self()),
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
    [VM] = [chunter_zoneparser:load([{name,Name},{state, VMState},{zonepath, Path},{uuid, UUID},{type, Type}]) || 
	       [ID,Name,VMState,Path,UUID,Type,_IP,_SomeNumber] <- 
		   [ re:split(Line, ":") 
		     || Line <- re:split(os:cmd("/usr/sbin/zoneadm -u" ++ binary_to_list(ZUUID) ++ " list -p"), "\n")],
	       ID =/= <<"0">>],
    VM.

list_vms(Auth) ->
    {ok, AuthC} = libsnarl:user_cache(system, Auth),
    [chunter_zoneparser:load([{name,Name},{state, VMState},{zonepath, Path},{uuid, UUID},{type, Type}]) || 
	[ID,Name,VMState,Path,UUID,Type,_IP,_SomeNumber] <- 
	    [ re:split(Line, ":") 
	      || Line <- re:split(os:cmd("/usr/sbin/zoneadm list -ip"), "\n")],
	ID =/= <<"0">> andalso
	    libsnarl:allowed(system, AuthC, [vm, UUID, view]) == true].

get_dataset(UUID, Ds) ->
    read_dsmanifest(filename:join(<<"/var/db/dsadm">>, <<UUID/binary, ".dsmanifest">>), Ds).

list_datasets(Datasets, Auth) ->
    {ok, AuthC} = libsnarl:user_cache(system, Auth),
    filelib:fold_files("/var/db/dsadm", ".*dsmanifest", false, 
		       fun (F, {Fs, DsA}) ->
			       UUID = re:run(F, "/var/db/dsadm/(.*)\.dsmanifest", 
					     [{capture, all_but_first, binary}]),
			       case libsnarl:allowed(system, AuthC, [vm, UUID, view]) of
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
