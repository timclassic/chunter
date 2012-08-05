%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 19 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_vm).

-behaviour(gen_server).

%% API
-export([start_link/1, 
	 refresh/1, 
	 get/1,
	 info/1,
	 set_state/2,
	 force_state/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {uuid, state=unknown, data, host}).

%%%===================================================================
%%% API
%%%===================================================================

set_state(Pid, State) ->
    gen_server:cast(Pid, {state, State}).

force_state(Pid, State) ->
    gen_server:cast(Pid, {force_state, State}).

refresh(Pid) ->
    gen_server:cast(Pid, refresh).

get(Pid) ->
    gen_server:call(Pid, get).

info(Pid) ->
    gen_server:call(Pid, info).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(UUID) ->
    gen_server:start_link(?MODULE, [UUID], []).

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
init([UUID]) ->
    ok = backyard_srv:register_connect_handler(backyard_connect),
    [Name|_] = re:split(os:cmd("uname -n"), "\n"),
    refresh(self()),
    {ok, #state{uuid=UUID, host=Name}}.

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
handle_call(info, _From, #state{uuid = UUID, data = Data} = State) ->
    case proplists:get_value(brand, Data) of
	<<"kvm">> ->
	    Info = chunter_vmadm:info(UUID),
	    {reply, {ok, Info}, State};
	_ ->
	    {reply, {error, not_supported}, State}
    end;

handle_call(get, _From, #state{data = Data, host = Host} = State) ->
    {reply, {ok, [{hypervisor, Host}|Data]}, State};
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
handle_cast(backyard_connect,  #state{uuid = UUID} = State) ->
    gproc:reg({p, l, {chunter, vm}}, UUID),
    gproc:reg({n, l, {vm, UUID}}, self()),
    {noreply, State};

handle_cast(refresh, #state{uuid=UUID} = State) ->
    Data = chunter_server:get_vm(UUID),
    {noreply, State#state{data = Data}};

handle_cast({force_state, MachineState}, #state{state = MachineState} = State) ->
    {noreply, State};

handle_cast({force_state, NewMachineState}, #state{uuid=UUID,
						   data=Data}=State) ->
    lager:info([{fifi_component, chunter},
		{vm, UUID}],
	       "[VM: ~s] State changed to ~s.~n", [UUID, NewMachineState]),
    try
	gproc:send({p,g,{vm,UUID}}, {vm, state, UUID, NewMachineState})
    catch
	_:_ ->
	    ok
    end,
    Data1 = [{state, list_to_binary(atom_to_list(NewMachineState))}|proplists:delete(state, Data)],
    {noreply, State#state{state=NewMachineState,
			  data=Data1}};

handle_cast({state, NewMachineState}, #state{state=OldMachineState}=State) ->
    case allowed_transitions(OldMachineState, NewMachineState) of
	true ->
	    force_state(self(), NewMachineState);
	false ->
	    ok
    end,
    {noreply, State};

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


%if we don't know yet everything is OK
allowed_transitions(unknown, _) ->
    true;
%default transitions
allowed_transitions(stopped, booting) ->
    true;
allowed_transitions(booting, running) ->
    true;
allowed_transitions(running, shutting_down) ->
    true;
allowed_transitions(shutting_down, stopped) ->
    true;
%escape if something runs off during powerup
allowed_transitions(booting, shutting_down) ->
    true;
allowed_transitions(_From, _To) ->
    false.


