-module(chunter_protocol).

-export([start_link/4, init/4]).

-ignore_xref([start_link/4]).


start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(ListenerPid),
    State = stateless,
    loop(Socket, Transport, State).

loop(Socket, Transport, HandlerState) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, BinData} ->
            case binary_to_term(BinData) of
                ping ->
                    Transport:send(Socket, term_to_binary(pong)),
                    ok = Transport:close(Socket);
                Data ->
                    case handle_message(Data, HandlerState) of
                        %% {reply, Reply, HandlerState1} ->
                        %%     Transport:send(Socket, term_to_binary({reply, Reply})),
                        %%     loop(Socket, Transport, HandlerState1);
                        %% {noreply, HandlerState1} ->
                        %%     Transport:send(Socket, term_to_binary(noreply))
                        %%         loop(Socket, Transport, HandlerState1);
                        {stop, Reply, _} ->
                            Transport:send(Socket, term_to_binary({reply, Reply})),
                            ok = Transport:close(Socket);
                        {stop, _} ->
                            ok = Transport:close(Socket)
                    end
            end;
        _ ->
            ok = Transport:close(Socket)
    end.


-spec handle_message(Message::fifo:chunter_message(), State::term()) -> {stop, term()}.

handle_message({machines, start, UUID}, State) when is_binary(UUID) ->
    chunter_vmadm:start(UUID),
    {stop, State};

handle_message({machines, update, UUID, Spec}, State) when is_binary(UUID) ->
    chunter_vm_fsm:update(UUID, Spec),
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
