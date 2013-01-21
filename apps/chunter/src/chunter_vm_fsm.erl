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
         update/2,
         snapshot/2,
         delete_snapshot/2,
         rollback_snapshot/2,
         force_state/2]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% This functions have to be exported but are only used internally.
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

update(UUID, Data) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}}, {update, Data}).

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


snapshot(UUID, SnapID) ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}}, {snapshot, SnapID}).

delete_snapshot(UUID, SnapID) ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}}, {snapshot, delete, SnapID}).

rollback_snapshot(UUID, SnapID) ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}}, {snapshot, rollback, SnapID}).

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

-spec initialized(Action::lad |
                          {create,  PackageSpec::fifo:package(),
                           DatasetSpec::fifo:dataset(), VMSpec::fifo:config()}, State::term()) ->
                         {next_state, loading, State::term()} |
                         {next_state, creating, State::term()} |
                         {next_state, initialized, State::term()}.
initialized(load, State) ->
    {next_state, loading, State};

initialized({create, PackageSpec, DatasetSpec, VMSpec}, State=#state{hypervisor = Hypervisor, uuid=UUID}) ->
    {ok, DatasetUUID} = jsxd:get(<<"dataset">>, DatasetSpec),
    VMData = chunter_spec:to_vmadm(PackageSpec, DatasetSpec, jsxd:set(<<"uuid">>, UUID, VMSpec)),
    SniffleData  = chunter_spec:to_sniffle(VMData),
    {ok, Ram} = jsxd:get(<<"ram">>, PackageSpec),
    SniffleData1 = jsxd:set(<<"ram">>, Ram, SniffleData),
    change_state(UUID, <<"installing_dataset">>),
    Info = chunter_vmadm:info(State#state.uuid),
    libhowl:send(UUID, [{<<"event">>, <<"update">>},
                        {<<"data">>,
                         [{<<"state">>, <<"installing_dataset">>},
                          {<<"hypervisor">>, Hypervisor},
                          {<<"config">>, SniffleData1}]}]),
    libsniffle:vm_set(UUID, [{<<"config">>, SniffleData1},
                             {<<"info">>, Info}]),
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
    libsniffle:vm_set(State#state.uuid, <<"state">>, NextState),
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
    libsniffle:vm_set(State#state.uuid, <<"info">>, Info),
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
        running = N ->
            Info = chunter_vmadm:info(State#state.uuid),
            libsniffle:vm_set(State#state.uuid, <<"info">>, Info),
            change_state(State#state.uuid, NextState, StateName =:= N),
            {next_state, running, State};
        Other ->
            change_state(State#state.uuid, NextState, StateName =:= Other),
            {next_state, Other, State}
    end;

handle_event(register, StateName, State) ->
    libsniffle:vm_register(State#state.uuid, State#state.hypervisor),
%%    change_state(State#state.uuid, atom_to_binary(StateName)),
    case load_vm(State#state.uuid) of
        {error, not_found} ->
            {stop, not_found, State};
        VMData ->
            Info = chunter_vmadm:info(State#state.uuid),
            libsniffle:vm_set(State#state.uuid, [{<<"state">>, atom_to_binary(StateName)},
                                                 {<<"config">>, chunter_spec:to_sniffle(VMData)},
                                                 {<<"info">>, Info}]),
            {next_state, StateName, State}
    end;

handle_event({update, Data}, _StateName, State) ->
    spawn(chunter_vmadm, update, [Data, State#state.uuid]),
    {stop, normal, State};

handle_event(remove, _StateName, State) ->
    libsniffle:vm_unregister(State#state.uuid),
    {stop, normal, State};

handle_event(delete, StateName, State) ->
    case load_vm(State#state.uuid) of
        {error, not_found} ->
            {stop, not_found, State};
        VM ->
            %%   case libsnarl:group_get(system, <<"vm_", UUID/binary, "_owner">>) of
            %%       {ok, GUUID} ->
            %%           libsnarl:group_delete(system, GUUID);
            %%       _ ->
            %%           ok
            %%   end,
            {ok, Mem} = jsxd:get(<<"max_physical_memory">>, VM),
            spawn(chunter_vmadm, delete, [State#state.uuid, Mem]),
            libhowl:send(State#state.uuid, [{<<"event">>, <<"delete">>}]),
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
handle_sync_event({snapshot, UUID}, _From, StateName, State) ->
    case load_vm(State#state.uuid) of
        {error, not_found} ->
            ok;
        VMData ->
            Spec = chunter_spec:to_sniffle(VMData),
            case jsxd:get(<<"zonepath">>, Spec) of
                {ok, P} ->
                    case do_snapshot(P, UUID) of
                        {ok, Reply} ->
                            R = lists:foldl(
                                  fun (Disk, {S, Reply0}) ->
                                          case jsxd:get(<<"path">>, Disk) of
                                              {ok, P1} ->
                                                  case do_snapshot(P1, UUID) of
                                                      {ok, Res} ->
                                                          {S, <<Reply0/binary, "\n", Res/binary>>};
                                                      {error, _Code, Res} ->
                                                          libsniffle:vm_log(
                                                            State#state.uuid,
                                                            <<"Failed to snapshot disk ", P1/binary, ": ", Reply/binary>>),
                                                          {error, <<Reply0/binary, "\n", Res/binary>>}
                                                  end;
                                              _ ->
                                                  {error, missing}
                                          end
                                  end, {ok, Reply}, jsxd:get(<<"disks">>, [], Spec)),
                            case R of
                                {ok, Res} ->
                                    libsniffle:vm_log(State#state.uuid, <<"Snapshot done ", Res/binary>>),
                                    {reply, ok, StateName, State};
                                {error, _} ->
                                    {reply, error, StateName, State}
                            end;
                        {error, _Code, Reply} ->
                            libsniffle:vm_log(State#state.uuid, <<"Failed to snapshot: ", Reply/binary>>),
                            {reply, error, StateName, State}
                    end;
                _ ->
                    libsniffle:vm_log(State#state.uuid, <<"Failed to snapshot: can't find zonepath.">>),
                    {reply, error, StateName, State}
            end
    end;

handle_sync_event({snapshot, delete, UUID}, _From, StateName, State) ->
    case load_vm(State#state.uuid) of
        {error, not_found} ->
            ok;
        VMData ->
            Spec = chunter_spec:to_sniffle(VMData),
            case jsxd:get(<<"zonepath">>, Spec) of
                {ok, P} ->
                    case do_delete_snapshot(P, UUID) of
                        {ok, Reply} ->
                            R = lists:foldl(
                                  fun (Disk, {S, Reply0}) ->
                                          case jsxd:get(<<"path">>, Disk) of
                                              {ok, P1} ->
                                                  case do_delete_snapshot(P1, UUID) of
                                                      {ok, Res} ->
                                                          {S, <<Reply0/binary, "\n", Res/binary>>};
                                                      {error, _Code, Res} ->
                                                          libsniffle:vm_log(
                                                            State#state.uuid,
                                                            <<"Failed to delete snapshot disk ", P1/binary, ": ", Reply/binary>>),
                                                          {error, <<Reply0/binary, "\n", Res/binary>>}
                                                  end;
                                              _ ->
                                                  {error, missing}
                                          end
                                  end, {ok, Reply}, jsxd:get(<<"disks">>, [], Spec)),
                            case R of
                                {ok, Res} ->
                                    libsniffle:vm_log(State#state.uuid, <<"Snapshot delete done ", Res/binary>>),
                                    {reply, ok, StateName, State};
                                {error, _} ->
                                    {reply, error, StateName, State}
                            end;
                        {error, _Code, Reply} ->
                            libsniffle:vm_log(State#state.uuid, <<"Failed to delete snapshot: ", Reply/binary>>),
                            {reply, error, StateName, State}
                    end;
                _ ->
                    libsniffle:vm_log(State#state.uuid, <<"Failed to delete snapshot: can't find zonepath.">>),
                    {reply, error, StateName, State}
            end
    end;

handle_sync_event({snapshot, rollback, UUID}, _From, StateName, State) ->
    case load_vm(State#state.uuid) of
        {error, not_found} ->
            ok;
        VMData ->
            Spec = chunter_spec:to_sniffle(VMData),
            case jsxd:get(<<"zonepath">>, Spec) of
                {ok, P} ->
                    case do_rollback_snapshot(P, UUID) of
                        {ok, Reply} ->
                            R = lists:foldl(
                                  fun (Disk, {S, Reply0}) ->
                                          case jsxd:get(<<"path">>, Disk) of
                                              {ok, P1} ->
                                                  case do_rollback_snapshot(P1, UUID) of
                                                      {ok, Res} ->
                                                          {S, <<Reply0/binary, "\n", Res/binary>>};
                                                      {error, _Code, Res} ->
                                                          libsniffle:vm_log(
                                                            State#state.uuid,
                                                            <<"Failed to rollback snapshot disk ", P1/binary, ": ", Reply/binary>>),
                                                          {error, <<Reply0/binary, "\n", Res/binary>>}
                                                  end;
                                              _ ->
                                                  {error, missing}
                                          end
                                  end, {ok, Reply}, jsxd:get(<<"disks">>, [], Spec)),
                            case R of
                                {ok, Res} ->
                                    libsniffle:vm_log(State#state.uuid, <<"Snapshot ", UUID/binary, " rollback done ", Res/binary>>),
                                    {reply, ok, StateName, State};
                                {error, _} ->
                                    {reply, error, StateName, State}
                            end;
                        {error, _Code, Reply} ->
                            libsniffle:vm_log(State#state.uuid, <<"Failed to rollback snapshot: ", Reply/binary>>),
                            {reply, error, StateName, State}
                    end;
                _ ->
                    libsniffle:vm_log(State#state.uuid, <<"Failed to rollback snapshot: can't find zonepath.">>),
                    {reply, error, StateName, State}
            end
    end;

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
    change_state(UUID, State, true).

-spec change_state(UUID::binary(), State::fifo:vm_state(), true | false) -> ok.

change_state(UUID, State, true) ->
    libsniffle:vm_log(UUID, <<"Transitioning ", State/binary>>),
    libsniffle:vm_set(UUID, <<"state">>, State),
    libhowl:send(UUID, [{<<"event">>, <<"state">>}, {<<"data">>, State}]);

change_state(UUID, State, false) ->
    libsniffle:vm_set(UUID, <<"state">>, State),
    libhowl:send(UUID, [{<<"event">>, <<"state">>}, {<<"data">>, State}]).


-spec binary_to_atom(B::binary()) -> A::atom().
binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).

-spec atom_to_binary(I::binary()|atom()) -> A::binary().
atom_to_binary(B) when is_binary(B) ->
    B;
atom_to_binary(A) ->
    list_to_binary(atom_to_list(A)).

do_snapshot(Path, SnapID) ->
    <<_:1/binary, P/binary>> = Path,
    CmdB = <<"/usr/sbin/zfs snapshot ",
             P/binary, "@", SnapID/binary>>,
    Cmd = binary_to_list(CmdB),
    lager:info("Creating snapshot: ~s", [Cmd]),
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000}, stderr_to_stdout, exit_status]),
    wait_for_port(Port, <<>>).

do_delete_snapshot(Path, SnapID) ->
    <<_:1/binary, P/binary>> = Path,
    CmdB = <<"/usr/sbin/zfs destroy ",
             P/binary, "@", SnapID/binary>>,
    Cmd = binary_to_list(CmdB),
    lager:info("Deleting snapshot: ~s", [Cmd]),
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000}, stderr_to_stdout, exit_status]),
    wait_for_port(Port, <<>>).

do_rollback_snapshot(Path, SnapID) ->
    <<_:1/binary, P/binary>> = Path,
    CmdB = <<"/usr/sbin/zfs rollback -r ",
             P/binary, "@", SnapID/binary>>,
    Cmd = binary_to_list(CmdB),
    lager:info("Deleting snapshot: ~s", [Cmd]),
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000}, stderr_to_stdout, exit_status]),
    wait_for_port(Port, <<>>).

wait_for_port(Port, Reply) ->
    receive
        {Port, {data, {eol, Data}}} ->
            wait_for_port(Port, <<Reply/binary, Data/binary>>);
        {Port, {data, Data}} ->
            wait_for_port(Port, <<Reply/binary, Data/binary>>);
        {Port,{exit_status, 0}} ->
            {ok, Reply};
        {Port,{exit_status, S}} ->
            {error, S, Reply}
    end.

