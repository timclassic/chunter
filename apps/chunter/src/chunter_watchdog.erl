%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 11 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_watchdog).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {name, 
		zoneport, 
		vmstat_port,
		mpstat_port,
		vfsstat_port,
		mpstat=undefiend,
		statspec=[], memory=0}).

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
    [Name|_] = re:split(os:cmd("uname -n"), "\n"),
    lager:info("chunter:watchdog - initializing: ~s", [Name]),
    Zonemon = code:priv_dir(chunter) ++ "/zonemon.sh",
    timer:send_interval(1000, zonecheck),
    PortOpts = [exit_status, use_stdio, binary, {line, 1000}],
    ZonePort = erlang:open_port({spawn, Zonemon}, PortOpts),
    lager:info("chunter:watchdog - zone watchdog started.", []),
    VMStat = code:priv_dir(chunter) ++ "/vmstat.sh",
    VMStatPort = erlang:open_port({spawn, VMStat}, PortOpts),
    MPStat = code:priv_dir(chunter) ++ "/mpstat.sh",
    MPStatPort = erlang:open_port({spawn, MPStat}, PortOpts),
    VFSStat = code:priv_dir(chunter) ++ "/vfsstat.sh",
    VFSStatPort = erlang:open_port({spawn, VFSStat}, PortOpts),

    lager:info("chunter:watchdog - stats watchdog started.", []),
    {Mem, _} = string:to_integer(os:cmd("/usr/sbin/prtconf | grep Memor | awk '{print $3}'")),
    {ok, #state{
       memory=Mem*1024,
       name=Name,
       zoneport=ZonePort,
       vmstat_port=VMStatPort,
       mpstat_port=MPStatPort,
       vfsstat_port=VFSStatPort
      }}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info(zonecheck, State) ->
    [chunter_vm:set_state(chunter_server:get_vm_pid(UUID),simplifie_state(list_to_atom((binary_to_list(VMState))))) ||
	[ID,_Name,VMState,_Path,UUID,_Type,_IP,_SomeNumber] <- 
	    [ re:split(Line, ":") 
	      || Line <- re:split(os:cmd("/usr/sbin/zoneadm list -ip"), "\n")],
	ID =/= <<"0">>],
    {noreply, State};

handle_info({_Port, {data, {eol, Data}}}, 
	    #state{vmstat_port=_Port, statspec=Spec, name=Name, memory=Mem, mpstat=MPStat} = State) ->
    case parse_stat(Data, Mem, Spec, MPStat) of
	unknown ->
	    lager:error("watchdog:stat - unknwon message: ~p", [Data]),
	    {noreply, State};
	skip ->
	    {noreply, State};
	{spec, NewSpec} ->
	    {noreply, State#state{statspec=NewSpec}};
	{stat, Stats} ->
	    try
		gproc:send({p,g,{host,Name}}, {host, stats, Name, Stats})
	    catch
		_:_ ->
		    ok
	    end,
	    {noreply, State#state{mpstat=[]}}
    end;
handle_info({_Port, {data, {eol, Data}}}, 
	    #state{name=Name, mpstat_port=_Port, mpstat=MPStat} = State) ->
    case parse_mpstat(Data) of
	{stats, {[CPU, 
		  Minf, Mjf, Xcal, 
		  Intr, Ithr, Csw, 
		  Icsw, Migr, Smtx, 
		  Srw, Syscl, Usr, 
		  Sys, _Wt, Idl], 
		 Stats}} ->
	    CPUS = integer_to_list(CPU),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".minf"], Minf, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".mjf"], Mjf, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".xcal"], Xcal, 1),

	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".intr"], Intr, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".Ithr"], Ithr, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".Csw"], Csw, 1),

	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".intr"], Icsw, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".migr"], Migr, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".smtx"], Smtx, 1),

	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".srw"], Srw, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".syscl"], Syscl, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".usr"], Usr, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".sys"], Sys, 1),
	    statsderl:gauge([Name, ".hypervisor.cpu.", CPUS, ".idl"], Idl, 1),

	    {noreply, State#state{mpstat=[Stats|MPStat]}};
	_ ->
	    {noreply, State}
    end;

handle_info({_Port, {data, {eol, Data}}}, #state{name=Name, zoneport=_Port} = State) ->
    case parse_data(Data) of
	{error, unknown} ->
	    statsderl:increment([Name, ".vm.zonewatchdog_error"], 1, 1),
	    lager:error("watchdog:zone - unknwon message: ~p", [Data]);
	{UUID, crate} ->
	    statsderl:increment([Name, ".vm.create"], 1, 1),
	    chunter_vm_sup:start_child(UUID);
	{UUID, Action} ->
	    statsderl:increment([Name, ".vm.", UUID, ".state_change"], 1, 1),
	    Pid = chunter_server:get_vm_pid(UUID),
	    chunter_vm:set_state(Pid, simplifie_state(Action))
    end,
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
terminate(_Reason, #state{vmstat_port=VMSPort,
			  mpstat_port=MPSPort,
			  vfsstat_port=VFSSPort,
			  zoneport=Zport}) ->
    erlang:port_close(VMSPort),
    erlang:port_close(MPSPort),
    erlang:port_close(VFSSPort),
    erlang:port_close(Zport),
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

simplifie_state(installed) ->
    stopped;
simplifie_state(uninitialized) ->
    stopped;
simplifie_state(initialized) ->
    booting;
simplifie_state(ready) ->
    booting;
simplifie_state(booting) ->
    booting;
simplifie_state(running) ->
    running;
simplifie_state(shutting_down) ->
    shutting_down;
simplifie_state(empty) ->
    shutting_down;
simplifie_state(down) -> 
    shutting_down;
simplifie_state(dying) -> 
    shutting_down;
simplifie_state(dead) -> 
    stopped.

parse_data(<<"S00: ", UUID/binary>>) ->
    {UUID, uninitialized};
parse_data(<<"S01: ", UUID/binary>>) ->
    {UUID, initialized};
parse_data(<<"S02: ", UUID/binary>>) ->
    {UUID, ready};
parse_data(<<"S03: ", UUID/binary>>) ->
    {UUID, booting};
parse_data(<<"S04: ", UUID/binary>>) ->
    {UUID, running};
parse_data(<<"S05: ", UUID/binary>>) ->
    {UUID, shutting_down};
parse_data(<<"S06: ", UUID/binary>>) ->
    {UUID, empty};
parse_data(<<"S07: ", UUID/binary>>) ->
    {UUID, down};
parse_data(<<"S08: ", UUID/binary>>) ->
    {UUID, dying};
parse_data(<<"S09: ", UUID/binary>>) ->
    {UUID, dead};
parse_data(<<"S10: ", UUID/binary>>) ->
    {UUID, uninitialized};
parse_data(<<"S11: ", UUID/binary>>) ->
    {UUID, creating};
parse_data(<<"S12: ", UUID/binary>>) ->
    {UUID, destroying};
parse_data(_) ->
    {error, unknown}.

parse_mpstat(<<"CPU", _R/binary>>) ->
    new;
parse_mpstat(D) ->
    Values = case re:split(D, "\s+") of
		 
		 [<<>> | Res] ->
		     Res;
		 Res ->
		     Res
	     end,
    [CPU, Minf, Mjf, Xcal, Intr, Ithr, Csw, Icsw, Migr, Smtx, Srw, Syscl, Usr, Sys, _Wt, Idl] = 
	[V || {V,_} <- [string:to_integer(binary_to_list(R)) || R <- Values]],
    {stats, {
       [CPU, Minf, Mjf, Xcal, Intr, Ithr, Csw, Icsw, Migr, Smtx, Srw, Syscl, Usr, Sys, _Wt, Idl],
       [{cpi_id, CPU},
	{minor_faults, Minf},
	{major_faults, Mjf},
	{chross_calls, Xcal},
	{interrupts, Intr},
	{thread_interrupts, Ithr},
	{context_switches, Csw},
	{involuntary_context_switches, Icsw},
	{thread_migrations, Migr},
	{mutexe_spins, Smtx},
	{rw_spins, Srw},
	{system_calls, Syscl},
	{user, Usr},
	{system, Sys},
	{idel, Idl}]}}.

parse_stat(<<" k", _R/binary>>, _, _, _) ->
    skip;
parse_stat(<<" r ", Specs/binary>>, _, _, _) ->
    {spec, [<<"r">> | re:split(Specs, "\s+")]};
parse_stat(<<" ", Data/binary>>, Mem, Specs, MPStat) ->
    Res = re:split(Data, "\s+"),
    Vals = [V || {V,_} <- [string:to_integer(binary_to_list(R)) || R <- Res]],
    {stat, build_stat(Specs, Vals, Mem, MPStat)};

parse_stat(_, _, _, _) ->
    unknown.

build_stat(S, D, Mem, MPStat) ->
    build_stat(S, D, kthr, [], [{total, Mem}], [], [], [], [{details, MPStat}]).

build_stat([], _, _Cat, K, M, P, D, F, C) ->
    [{kthr, K},
     {memory, M},
     {page, P},
     {disk, D},
     {faults, F},
     {cpu, C}];
build_stat(_, [], _Cat, K, M, P, D, F, C) ->
    [{kthr, K},
     {memory, M},
     {page, P},
     {disk, D},
     {faults, F},
     {cpu, C}];

build_stat([<<"r">>|R], [V|RV], kthr, K, M, P, D, F, C) ->
    build_stat(R, RV, kthr, [{queue, V}|K], M, P, D, F, C);
build_stat([<<"b">>|R], [V|RV], kthr, K, M, P, D, F, C) ->
    build_stat(R, RV, kthr, [{blocked, V}|K], M, P, D, F, C);
build_stat([<<"w">>|R], [V|RV], kthr, K, M, P, D, F, C) ->
    build_stat(R, RV, memory, [{swapped, V}|K], M, P, D, F, C);

build_stat([<<"swap">>|R], [V|RV], memory, K, M, P, D, F, C) ->
    build_stat(R, RV, memory, K, [{swap, V}|M], P, D, F, C);
build_stat([<<"free">>|R], [V|RV], memory, K, M, P, D, F, C) ->
    build_stat(R, RV, page, K, [{free, V}|M], P, D, F, C);


build_stat([<<"re">>|R], [V|RV], page, K, M, P, D, F, C) ->
    build_stat(R, RV, page, K, M, [{reclaims, V}|P], D, F, C);
build_stat([<<"mf">>|R], [V|RV], page, K, M, P, D, F, C) ->
    build_stat(R, RV, page, K, M, [{minor_faults, V}|P], D, F, C);
build_stat([<<"pi">>|R], [V|RV], page, K, M, P, D, F, C) ->
    build_stat(R, RV, page, K, M, [{in, V}|P], D, F, C);
build_stat([<<"po">>|R], [V|RV], page, K, M, P, D, F, C) ->
    build_stat(R, RV, page, K, M, [{out, V}|P], D, F, C);
build_stat([<<"fr">>|R], [V|RV], page, K, M, P, D, F, C) ->
    build_stat(R, RV, page, K, M, [{freed, V}|P], D, F, C);
build_stat([<<"de">>|R], [V|RV], page, K, M, P, D, F, C) ->
    build_stat(R, RV, page, K, M, [{memory_shortfall, V}|P], D, F, C);
build_stat([<<"sr">>|R], [V|RV], page, K, M, P, D, F, C) ->
    build_stat(R, RV, disk, K, M, [{scanned, V}|P], D, F, C);

build_stat([<<"in">>|R], [V|RV], disk, K, M, P, D, F, C) ->
    build_stat(R, RV, faults, K, M, P, D, [{interrupts, V}|F], C);
build_stat([_|R], [V|RV], disk, K, M, P, D, F, C) ->
    build_stat(R, RV, disk, K, M, P, [V|D], F, C);

build_stat([<<"sy">>|R], [V|RV], faults, K, M, P, D, F, C) ->
    build_stat(R, RV, faults, K, M, P, D, [{system_calls, V}|F], C);
build_stat([<<"cs">>|R], [V|RV], faults, K, M, P, D, F, C) ->
    build_stat(R, RV, cpu, K, M, P, D, [{context_switches, V}|F], C);

build_stat([<<"us">>|R], [V|RV], cpu, K, M, P, D, F, C) ->
    build_stat(R, RV, cpu, K, M, P, D, F, [{user, V}|C]);
build_stat([<<"sy">>|R], [V|RV], cpu, K, M, P, D, F, C) ->
    build_stat(R, RV, cpu, K, M, P, D, F, [{system, V}|C]);
build_stat([<<"id">>|R], [V|RV], cpu, K, M, P, D, F, C) ->
    build_stat(R, RV, cpu, K, M, P, D, F, [{idel, V}|C]).
