-module(chunter_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/4]).

-record(state, {socket,
                transport,
                ok,
                error,
                closed,
                type = normal,
                state = undefined}).

start_link(ListenerPid, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [[ListenerPid, Socket, Transport, Opts]]).

init([ListenerPid, Socket, Transport, _Opts]) ->
    ok = proc_lib:init_ack({ok, self()}),
    %% Perform any required state initialization here.
    ok = ranch:accept_ack(ListenerPid),
    ok = Transport:setopts(Socket, [{active, true}, {packet,4}]),
    {OK, Closed, Error} = Transport:messages(),
    gen_server:enter_loop(?MODULE, [], #state{
                                     ok = OK,
                                     closed = Closed,
                                     error = Error,
                                     socket = Socket,
                                     transport = Transport}).

handle_info({data,Data}, State = #state{socket = Socket,
                                        transport = Transport}) ->
    Transport:send(Socket, Data),
    {noreply, State};

handle_info({_Closed, _Socket}, State = #state{
                                  type = mornal,
                                  closed = _Closed}) ->
    {stop, normal, State};

handle_info({_OK, Socket, BinData}, State = #state{
                                      type = normal,
                                      transport = Transport,
                                      ok = _OK}) ->
    Msg = binary_to_term(BinData),
    lager:info("Got binary message: ~p.", [Msg]),
    case Msg of
        {dtrace, Script} ->
            lager:debug("Compiling DTrace script: ~p.", [Script]),
            {ok, Handle} = erltrace:open(),
            ok = erltrace:compile(Handle, Script),
            ok = erltrace:go(Handle),
            lager:debug("DTrace running.", [Script]),
            {noreply, State#state{state = Handle,
                                  type = dtrace}};
        {console, UUID} ->
            lager:debug("Console: ~p.", [UUID]),
            chunter_vm_fsm:console_link(UUID, self()),
            {noreply, State#state{state = UUID,
                                  type = console}};
        ping ->
            lager:debug("Ping.", []),
            Transport:send(Socket, term_to_binary(pong)),
            ok = Transport:close(Socket),
            {stop, normal, State};
        Data ->
            lager:debug("Default message: ~p.", [Data]),
            case handle_message(Data, undefined) of
                {stop, Reply, _} ->
                    Transport:send(Socket, term_to_binary({reply, Reply})),
                    Transport:close(Socket),
                    {stop, normal, State};
                {stop, _} ->
                    ok = Transport:close(Socket),
                    {stop, normal, State}
            end
    end;

handle_info({_OK, Socket, BinData},  State = #state{
                                       state = Handle,
                                       type = dtrace,
                                       transport = Transport,
                                       ok = _OK}) ->
    case binary_to_term(BinData) of
        stop ->
            erltrace:stop(Handle);
        go ->
            erltrace:go(Handle);
        Act ->
            Res = case Act of
                      walk ->
                          erltrace:walk(Handle);
                      consume ->
                          erltrace:consume(Handle)
                  end,
            lager:debug("Reading from Dtrace(~p): ~p", [Act, Res]),
            Transport:send(Socket, term_to_binary(Res))
    end,
    {noreply, State};

handle_info({_OK, _S, Data}, State = #state{
                               type = console,
                               state = UUID,
                               ok = _OK}) ->
    chunter_vm_fsm:console_send(UUID, Data),
    {noreply, State};

handle_info({_Closed, _}, State = #state{ closed = _Closed}) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_message(Message::fifo:chunter_message(), State::term()) -> {stop, term()}.

handle_message({machines, start, UUID}, State) when is_binary(UUID) ->
    chunter_vmadm:start(UUID),
    {stop, State};

handle_message({machines, update, UUID, Package, Config}, State) when is_binary(UUID) ->
    chunter_vm_fsm:update(UUID, Package, Config),
    {stop, State};

handle_message({machines, start, UUID, Image}, State) when is_binary(UUID),
                                                           is_binary(Image) ->
    chunter_vmadm:start(UUID, Image),
    {stop, State};

handle_message({machines, snapshot, UUID, SnapId}, State) when is_binary(UUID),
                                                               is_binary(SnapId) ->
    {stop, chunter_vm_fsm:snapshot(UUID, SnapId), State};

handle_message({machines, snapshot, delete, UUID, SnapId}, State) when is_binary(UUID),
                                                                       is_binary(SnapId) ->
    {stop, chunter_vm_fsm:delete_snapshot(UUID, SnapId), State};

handle_message({machines, snapshot, rollback, UUID, SnapId}, State) when is_binary(UUID),
                                                                         is_binary(SnapId) ->
    {stop, chunter_vm_fsm:rollback_snapshot(UUID, SnapId), State};

handle_message({machines, stop, UUID}, State) when is_binary(UUID) ->
    chunter_vmadm:stop(UUID),
    {stop, State};

handle_message({machines, reboot, UUID}, State) when is_binary(UUID) ->
    chunter_vmadm:reboot(UUID),
    {stop, State};

handle_message({machines, create, UUID, PSpec, DSpec, Config}, State) when is_binary(UUID),
                                                                           is_list(PSpec),
                                                                           is_list(DSpec),
                                                                           is_list(Config) ->
    chunter_vm_fsm:create(UUID, PSpec, DSpec, Config),
    {stop, State};

handle_message({machines, delete, UUID}, State) when is_binary(UUID) ->
    chunter_vm_fsm:delete(UUID),
    {stop, State};

handle_message(Oops, State) ->
    io:format("oops: ~p~n", [Oops]),
    {stop, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknwon}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
