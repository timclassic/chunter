%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_vm_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).
-ignore_xref([start_link/1,
              initialized/2,
              creating/2,
              loading/2,
              stopped/2,
              booting/2,
              running/2,
              shutting_down/2]).

-export([create/4,
         load/1,
         delete/1,
         remove/1,
         transition/2,
         force_state/2]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).


                                                % This functions have to be exported but are only used internally.
-export([initialized/2,
         creating/2,
         loading/2,
         stopped/2,
         booting/2,
         running/2,
         shutting_down/2]).

-define(SERVER, ?MODULE).

-record(state, {hypervisor, uuid}).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(UUID::fifo:uuid(), PackageSpec::fifo:package(),
             DatasetSpec::fifo:dataset(), VMSpec::fifo:config()) ->
                    ok.

create(UUID, PackageSpec, DatasetSpec, VMSpec) ->
    start_link(UUID),
    gen_fsm:send_event({global, {vm, UUID}}, {create, PackageSpec, DatasetSpec, VMSpec}).


-spec load(UUID::fifo:uuid()) -> ok.

load(UUID) ->
    case global:whereis_name({vm, UUID}) of
        undefined ->
            start_link(UUID),
            gen_fsm:send_event({global, {vm, UUID}}, load);
        _ ->
            register(UUID)
    end.

-spec transition(UUID::fifo:uuid(), State::fifo:vm_state()) -> ok.

transition(UUID, State) ->
    gen_fsm:send_event({global, {vm, UUID}}, {transition, State}).


-spec delete(UUID::fifo:uuid()) -> ok.

delete(UUID) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}}, delete).

-spec remove(UUID::fifo:uuid()) -> ok.

remove(UUID) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}}, remove).



-spec force_state(UUID::fifo:uuid(), State::fifo:vm_state()) -> ok.

force_state(UUID, State) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}}, {force_state, State}).

-spec register(UUID::fifo:uuid()) -> ok.

register(UUID) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}}, register).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(UUID) ->
    gen_fsm:start_link({global, {vm, UUID}}, ?MODULE, [UUID], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([UUID]) ->
    [Hypervisor|_] = re:split(os:cmd("uname -n"), "\n"),
    libsniffle:vm_register(UUID, Hypervisor),
    {ok, initialized, #state{uuid = UUID, hypervisor = Hypervisor}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

-spec initialized(Action::load |
                          {create,  PackageSpec::fifo:package(),
                           DatasetSpec::fifo:dataset(), VMSpec::fifo:config()}, State::term()) ->
                         {next_state, loading, State::term()} |
                         {next_state, creating, State::term()} |
                         {next_state, initialized, State::term()}.
initialized(load, State) ->
    {next_state, loading, State};

initialized({create, PackageSpec, DatasetSpec, VMSpec}, State=#state{hypervisor = Hypervisor, uuid=UUID}) ->
    {<<"dataset">>, DatasetUUID} = lists:keyfind(<<"dataset">>, 1, DatasetSpec),
    VMData = chunter_spec:to_vmadm(PackageSpec, DatasetSpec, [{<<"uuid">>, UUID} | VMSpec]),
    SniffleData  = chunter_spec:to_sniffle(VMData),
    {<<"ram">>, Ram} = lists:keyfind(<<"ram">>, 1, VMData),
    SniffleData1 = lists:keydelete(<<"ram">>, 1, SniffleData),
    SniffleData2 = [{<<"ram">>, Ram} | SniffleData1],
    change_state(UUID, <<"installing_dataset">>),
    libhowl:send(UUID, [{<<"event">>, <<"update">>},
                        {<<"data">>,
                         [{<<"state">>, <<"installing_dataset">>},
                          {<<"hypervisor">>, Hypervisor},
                          {<<"config">>, SniffleData2}]}]),
    libsniffle:vm_attribute_set(UUID, <<"config">>, SniffleData2),
    install_image(DatasetUUID),
    spawn(chunter_vmadm, create, [VMData]),
    change_state(UUID, <<"creating">>),
    {next_state, creating, State};

initialized(_, State) ->
    {next_state, initialized, State}.


-spec creating({transition, NextState::fifo:vm_state()}, State::term()) ->
                      {next_state, atom(), State::term()}.

creating({transition, NextState}, State) ->
    change_state(State#state.uuid, NextState),
    {next_state, binary_to_atom(NextState), State}.

-spec loading({transition, NextState::fifo:vm_state()}, State::term()) ->
                     {next_state, atom(), State::term()}.

loading({transition, NextState}, State) ->
    libsniffle:vm_attribute_set(State#state.uuid, <<"state">>, NextState),
    {next_state, binary_to_atom(NextState), State}.

-spec stopped({transition, NextState::fifo:vm_state()}, State::term()) ->
                     {next_state, atom(), State::term()}.

stopped({transition, NextState = <<"booting">>}, State) ->
    change_state(State#state.uuid, NextState),
    {next_state, binary_to_atom(NextState), State};

stopped(start, State) ->
    chunter_vmadm:start(State#state.uuid),
    {next_state, stopped, State};

stopped(_, State) ->
    {next_state, stopped, State}.

-spec booting({transition, NextState::fifo:vm_state()}, State::term()) ->
                     {next_state, atom(), State::term()}.

booting({transition, NextState = <<"shutting_down">>}, State) ->
    change_state(State#state.uuid, NextState),
    {next_state, binary_to_atom(NextState), State};

booting({transition, NextState = <<"running">>}, State) ->
    change_state(State#state.uuid, NextState),
    Info = chunter_vmadm:info(State#state.uuid),
    libsniffle:vm_attribute_set(State#state.uuid, <<"info">>, Info),
    {next_state, binary_to_atom(NextState), State};

booting(_, State) ->
    {next_state, booting, State}.

-spec running({transition, NextState::fifo:vm_state()}, State::term()) ->
                     {next_state, atom(), State::term()}.

running({transition, NextState = <<"shutting_down">>}, State) ->
    change_state(State#state.uuid, NextState),
    {next_state, binary_to_atom(NextState), State};

running(_, State) ->
    {next_state, running, State}.

-spec shutting_down({transition, NextState::fifo:vm_state()}, State::term()) ->
                           {next_state, atom(), State::term()}.

shutting_down({transition, NextState = <<"stopped">>}, State) ->
    change_state(State#state.uuid, NextState),
    {next_state, binary_to_atom(NextState), State};

shutting_down(_, State) ->
    {next_state, shutting_down, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

-spec handle_event({force_state, NextState::fifo:vm_state()},
                   StateName::atom(),
                   State::term()) ->
                          {next_state, NextStateName::fifo:vm_state_atom(), NextState::term()} |
                          {stop, Reason::term(), NewState::term()}.

handle_event({force_state, NextState}, StateName, State) ->
    case binary_to_atom(NextState) of
        StateName ->
            {next_state, StateName, State};
        Other ->
            change_state(State#state.uuid, NextState),
            {next_state, Other, State}
    end;

handle_event(register, StateName, State) ->
    libsniffle:vm_register(State#state.uuid, State#state.hypervisor),
    change_state(State#state.uuid, atom_to_binary(StateName)),
    case load_vm(State#state.uuid) of
        {error, not_found} ->
            {stop, not_found, State};
        VMData ->
            libsniffle:vm_attribute_set(State#state.uuid, <<"config">>, chunter_spec:to_sniffle(VMData)),
            {next_state, StateName, State}
    end;

handle_event(remove, _StateName, State) ->
    libsniffle:vm_unregister(State#state.uuid),
    {stop, normal, State};

handle_event(delete, StateName, State) ->
    case load_vm(State#state.uuid) of
        {error, not_found} ->
            {stop, not_found, State};
        VM ->
            case proplists:get_value(<<"nics">>, VM) of
                undefined ->
                    [];
                Nics ->
                    [try
                         Net = proplists:get_value(<<"nic_tag">>, Nic),
                         IP = proplists:get_value(<<"ip">>, Nic),
                         libsniffle:iprange_release(Net, libsniffle:ip_to_int(IP)),
                         ok
                     catch
                         _:_ ->
                             ok
                     end
                     || Nic <- Nics]
            end,
                                                %    case libsnarl:group_get(system, <<"vm_", UUID/binary, "_owner">>) of
                                                %   {ok, GUUID} ->
                                                %       libsnarl:group_delete(system, GUUID);
                                                %   _ ->
                                                %       ok
                                                %   end,
            {<<"max_physical_memory">>, Mem} = lists:keyfind(<<"max_physical_memory">>, 1, VM),
            spawn(chunter_vmadm, delete, [State#state.uuid, Mem]),
            {next_state, StateName, State}
    end;


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec install_image(DatasetUUID::fifo:uuid()) -> ok | string().

install_image(DatasetUUID) ->
    case filelib:is_regular(filename:join(<<"/var/db/imgadm">>, <<DatasetUUID/binary, ".json">>)) of
        true ->
            ok;
        false ->
            os:cmd("/usr/sbin/imgadm update"),
            os:cmd(binary_to_list(<<"/usr/sbin/imgadm import ", DatasetUUID/binary>>))

    end.


-spec zoneadm(ZUUID::fifo:uuid()) -> [{ID::binary(),
                                       Name::binary(),
                                       VMState::binary(),
                                       Path::binary(),
                                       UUID::binary(),
                                       Type::binary()}].

zoneadm(ZUUID) ->
    Zones = [ re:split(Line, ":")
              || Line <- re:split(os:cmd("/usr/sbin/zoneadm -u" ++ binary_to_list(ZUUID) ++ " list -p"), "\n")],
    [{ID, Name, VMState, Path, UUID, Type} ||
        [ID, Name, VMState, Path, UUID, Type, _IP, _SomeNumber] <- Zones].

-spec load_vm(ZUUID::fifo:uuid()) -> fifo:vm_config() | {error, not_found}.

load_vm(ZUUID) ->
    case [chunter_zoneparser:load([{<<"name">>,Name},
                                   {<<"state">>, VMState},
                                   {<<"zonepath">>, Path},
                                   {<<"type">>, Type}]) ||
             {_ID, Name, VMState, Path, _UUID, Type} <- zoneadm(ZUUID)] of
        [VM | _] ->
            VM;
        [] ->
            {error, not_found}
    end.



-spec change_state(UUID::binary(), State::fifo:vm_state()) -> ok.

change_state(UUID, State) ->
    libsniffle:vm_attribute_set(UUID, <<"state">>, State),
    libhowl:send(UUID, [{<<"event">>, <<"state">>}, {<<"data">>, State}]).


-spec binary_to_atom(B::binary()) -> A::atom().
binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).

-spec atom_to_binary(I::binary()|atom()) -> A::binary().
atom_to_binary(B) when is_binary(B) ->
    B;
atom_to_binary(A) ->
    list_to_binary(atom_to_list(A)).
