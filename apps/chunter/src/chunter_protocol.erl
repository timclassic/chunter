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
    ok = Transport:setopts(Socket, [{active, true}, {packet,4}, {nodelay, true}]),
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
    case Msg of
        {dtrace, Script} ->
            lager:info("Compiling DTrace script: ~p.", [Script]),
            {ok, Handle} = erltrace:open(),
            ok = erltrace:compile(Handle, Script),
            ok = erltrace:go(Handle),
            lager:info("DTrace running."),
            {noreply, State#state{state = Handle,
                                  type = dtrace}};
        {console, UUID} ->
            lager:info("Console: ~p.", [UUID]),
            chunter_vm_fsm:console_link(UUID, self()),
            {noreply, State#state{state = UUID,
                                  type = console}};
        ping ->
            lager:debug("Ping."),
            Transport:send(Socket, term_to_binary(pong)),
            ok = Transport:close(Socket),
            {stop, normal, State};
        Data ->
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
        {Act, Ref, Fn} ->
            lager:info("<~p> Starting ~p.", [Ref, Act]),
            Transport:send(Socket, term_to_binary({ok, Ref})),
            {Time, Res} = timer:tc(fun() ->
                                           case Act of
                                               walk ->
                                                   erltrace:walk(Handle);
                                               consume ->
                                                   erltrace:consume(Handle)
                                           end
                                   end),
            {Time1, Res1} = timer:tc(fun () ->
                                             case Res of
                                                 {ok, D} ->
                                                     case Fn of
                                                         llquantize ->
                                                             {ok, llquantize(D)};
                                                         identity ->
                                                             {ok, D}
                                                     end;
                                                 D ->
                                                     D
                                             end
                                     end),
            Now = now(),
            Transport:send(Socket, term_to_binary(Res1)),
            lager:info("<~p> Dtrace ~p  took ~pus + ~pus + ~pus.", [Ref, Act, Time, Time1, timer:now_diff(now(), Now)])
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

-spec handle_message(Message::fifo:chunter_message(), State::term()) ->
                            {stop, term()} | {stop, term(), term()}.

handle_message({machines, start, UUID}, State) when is_binary(UUID) ->
    chunter_vmadm:start(UUID),
    {stop, State};

handle_message({machines, update, UUID, Package, Config}, State)
  when is_binary(UUID) ->
    chunter_vm_fsm:update(UUID, Package, Config),
    {stop, State};

handle_message({machines, start, UUID, Image}, State) when is_binary(UUID),
                                                           is_binary(Image) ->
    chunter_vmadm:start(UUID, Image),
    {stop, State};

handle_message({machines, backup, UUID, SnapId, Options}, State)
  when is_binary(UUID),
       is_binary(SnapId)->
    chunter_vm_fsm:backup(UUID, SnapId, Options),
    {stop, State};

handle_message({machines, backup, restore, UUID, SnapId, Options}, State)
  when is_binary(UUID),
       is_binary(SnapId)->
    chunter_vm_fsm:restore_backup(UUID, SnapId, Options),
    {stop, State};

handle_message({machines, snapshot, UUID, SnapId}, State)
  when is_binary(UUID),
       is_binary(SnapId) ->
    chunter_vm_fsm:snapshot(UUID, SnapId),
    {stop, State};

handle_message({machines, backup, delete, UUID, SnapId}, State)
  when is_binary(UUID),
       is_binary(SnapId) ->
    {stop, chunter_vm_fsm:delete_backup(UUID, SnapId), State};

handle_message({machines, service, enable, UUID, Service}, State)
  when is_binary(UUID),
       is_binary(Service) ->
    {stop, chunter_vm_fsm:service_action(UUID, enable, Service), State};


handle_message({machines, service, disable, UUID, Service}, State)
  when is_binary(UUID),
       is_binary(Service) ->
    {stop, chunter_vm_fsm:service_action(UUID, disable, Service), State};

handle_message({machines, service, clear, UUID, Service}, State)
  when is_binary(UUID),
       is_binary(Service) ->
    {stop, chunter_vm_fsm:service_action(UUID, clear, Service), State};

handle_message({machines, snapshot, delete, UUID, SnapId}, State)
  when is_binary(UUID),
       is_binary(SnapId) ->
    {stop, chunter_vm_fsm:delete_snapshot(UUID, SnapId), State};

handle_message({machines, snapshot, rollback, UUID, SnapId}, State)
  when is_binary(UUID),
       is_binary(SnapId) ->
    {stop, chunter_vm_fsm:rollback_snapshot(UUID, SnapId), State};

handle_message({machines, snapshot, upload,
                UUID, SnapId, Host, Port, Bucket, AKey, SKey, Bucket}, State)
  when is_binary(UUID),
       is_binary(SnapId) ->
    spawn(fun() ->
                  upload_snapshot(UUID, SnapId, Host, Port, Bucket, AKey,
                                  SKey, Bucket, [])
          end),
    {stop, ok, State};

handle_message({machines, snapshot, upload,
                UUID, SnapId, Host, Port, Bucket, AKey, SKey, Bucket, Options},
               State)
  when is_binary(UUID),
       is_binary(SnapId) ->
    spawn(fun() ->
                  upload_snapshot(UUID, SnapId, Host, Port, Bucket, AKey,
                                  SKey, Bucket, Options)
          end),
    {stop, ok, State};

handle_message({machines, snapshot, download,
                UUID, SnapId, Host, Port, Bucket, AKey, SKey, Bucket, Options},
               State)
  when is_binary(UUID),
       is_binary(SnapId) ->
    spawn(fun() ->
                  download_snapshot(UUID, SnapId, Host, Port, Bucket, AKey,
                                    SKey, Bucket, Options)
          end),
    {stop, ok, State};

handle_message({machines, snapshot, download,
                UUID, SnapId, Host, Port, Bucket, AKey, SKey, Bucket}, State)
  when is_binary(UUID),
       is_binary(SnapId) ->
    spawn(fun() ->
                  download_snapshot(UUID, SnapId, Host, Port, Bucket, AKey,
                                    SKey, Bucket, [])
          end),
    {stop, ok, State};

handle_message({machines, snapshot, store, UUID, SnapId, Img}, State)
  when is_binary(UUID),
       is_binary(SnapId) ->
    spawn(fun() ->
                  write_snapshot(UUID, SnapId, Img)
          end),
    {stop, ok, State};


handle_message({machines, stop, UUID}, State) when is_binary(UUID) ->
    chunter_vmadm:stop(UUID),
    {stop, State};

handle_message({machines, stop, force, UUID}, State) when is_binary(UUID) ->
    chunter_vmadm:force_stop(UUID),
    {stop, State};

handle_message({machines, reboot, UUID}, State) when is_binary(UUID) ->
    chunter_vmadm:reboot(UUID),
    {stop, State};

handle_message({machines, reboot, force, UUID}, State) when is_binary(UUID) ->
    chunter_vmadm:force_reboot(UUID),
    {stop, State};

handle_message({lock, UUID}, State) ->
    {stop, chunter_server:lock(UUID), State};

handle_message({machines, create, UUID, PSpec, DSpec, Config}, State)
  when is_binary(UUID), is_list(PSpec), is_list(DSpec), is_list(Config) ->
    case chunter_server:lock(UUID) of
        ok ->
            chunter_vm_fsm:create(UUID, PSpec, DSpec, Config),
            {stop, ok, State};
        _ ->
            {stop, {error, lock}, State}
    end;

handle_message({machines, delete, UUID}, State) when is_binary(UUID) ->
    chunter_vm_fsm:delete(UUID),
    {stop, State};


handle_message(update, State) ->
    lager:info("updating chunter", []),
    os:cmd("/opt/chunter/bin/update"),
    {stop, State};

handle_message(Oops, State) ->
    lager:info("oops: ~p~n", [Oops]),
    {stop, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknwon}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

llquantize(Data) ->
    lists:foldr(fun ({_, Path, Vals}, Obj) ->
                        BPath = lists:map(fun(L) when is_list(L) ->
                                                  list_to_binary(L);
                                             (B) when is_binary(B) ->
                                                  B;
                                             (N) when is_number(N) ->
                                                  list_to_binary(integer_to_list(N))
                                          end, Path),
                        lists:foldr(fun({{Start, End}, Value}, Obj1) ->
                                            B = list_to_binary(io_lib:format("~p-~p", [Start, End])),
                                            jsxd:set(BPath ++ [B], Value, Obj1)
                                    end, Obj, Vals)
                end, [], Data).


write_snapshot(UUID, SnapId, Img) ->
    Cmd = code:priv_dir(chunter) ++ "/zfs_send.gzip.sh",
    lager:debug("Running ZFS command: ~p ~s ~s", [Cmd, UUID, SnapId]),
    Port = open_port({spawn_executable, Cmd},
                     [{args, [UUID, SnapId]}, use_stdio, binary,
                      stderr_to_stdout, exit_status, stream]),
    libsniffle:dataset_set(Img, <<"imported">>, 0),
    write_snapshot(Port, Img, <<>>, 0).

write_snapshot(Port, Img, <<MB:1048576/binary, Acc/binary>>, Idx) ->
    lager:debug("<IMG> ~s[~p]", [Img, Idx]),
    libsniffle:img_create(Img, Idx, binary:copy(MB)),
    write_snapshot(Port, Img, Acc, Idx+1);

write_snapshot(Port, Img, Acc, Idx) ->
    receive
        {Port, {data, Data}} ->
            write_snapshot(Port, Img, <<Acc/binary, Data/binary>>, Idx);
        {Port,{exit_status, 0}} ->
            case Acc of
                <<>> ->
                    ok;
                _ ->
                    lager:debug("<IMG> ~s[~p]", [Img, Idx]),
                    libsniffle:img_create(Img, Idx, binary:copy(Acc))
            end,
            lager:info("Writing image ~s finished with ~p parts.", [Img, Idx]),
            libsniffle:dataset_set(Img, <<"imported">>, 1),
            ok;
        {Port,{exit_status, S}} ->
            lager:error("Writing image ~s failed after ~p parts with exit "
                        "status ~p.", [Img, Idx, S]),
            ok
    end.

upload_snapshot(UUID, SnapID, Host, Port, Bucket, AKey, SKey, Bucket, Options) ->
    Conf = fifo_s3:make_config(AKey, SKey, Host, Port),
    {ok, Upload} = fifo_s3:new_upload(Bucket, binary_to_list(SnapID), Conf),
    Cmd = code:priv_dir(chunter) ++ "/zfs_send.gzip.sh",
    SnapID1 = case proplists:is_defined(create, Options) of
                  true ->
                      uuid:uuid4s();
                  false ->
                      SnapID
              end,
    Prt = case proplists:get_value(parent, Options) of
              undefined ->
                  lager:debug("Running ZFS command: ~p ~s ~s",
                              [Cmd, UUID, SnapID1]),
                  open_port({spawn_executable, Cmd},
                            [{args, [UUID, SnapID1]}, use_stdio, binary,
                             stderr_to_stdout, exit_status, stream]);
              Inc ->
                  libsniffle:vm_set(
                    UUID, [<<"snapshots">>, SnapID1, <<"parent">>], Inc),
                  lager:debug("Running ZFS command: ~p ~s ~s ~s",
                              [Cmd, UUID, SnapID1, Inc]),
                  open_port({spawn_executable, Cmd},
                            [{args, [UUID, SnapID1, Inc]}, use_stdio, binary,
                             stderr_to_stdout, exit_status, stream])
          end,
    libsniffle:vm_set(
      UUID, [<<"snapshots">>, SnapID, <<"state">>], <<"uploading">>),
    upload_snapshot(UUID, SnapID, Prt, Upload, <<>>, 0,
                    proplists:is_defined(delete, Options)).


upload_snapshot(UUID, SnapID, Port, Upload, <<MB:1048576/binary, Acc/binary>>,
                Size, Delete) ->
    case fifo_s3:put_upload(binary:copy(MB), Upload) of
        {ok, Upload1} ->
            lager:debug("Uploading part: ~p.", [Size]),
            upload_snapshot(UUID, SnapID, Port, Upload1, Acc, Size, Delete);
        {error, E} ->
            fifo_s3:abort_upload(Upload),
            lager:error("Upload error: ~p", [E]),
            libsniffle:vm_set(
              UUID, [<<"snapshots">>, SnapID, <<"state">>],
              <<"upload failed">>),
            ok
    end;

upload_snapshot(UUID, SnapID, Port, Upload, Acc, Size, Delete) ->
    receive
        {Port, {data, Data}} ->
            Size1 = Size + byte_size(Data),
            upload_snapshot(UUID, SnapID, Port, Upload,
                            <<Acc/binary, Data/binary>>, Size1, Delete);
        {Port, {exit_status, 0}} ->
            case Acc of
                <<>> ->
                    ok;
                _ ->
                    case fifo_s3:put_upload(binary:copy(Acc), Upload) of
                        {ok, Upload1} ->
                            fifo_s3:complete_upload(Upload1),
                            lager:debug("Upload complete: ~p.", [Size]),
                            libsniffle:vm_set(
                              UUID, [<<"snapshots">>, SnapID, <<"state">>],
                              <<"uploaded">>),
                            libsniffle:vm_set(
                              UUID, [<<"snapshots">>, SnapID, <<"location">>],
                              <<"cloud">>),
                            case Delete of
                                true ->
                                    chunter_vm_fsm:delete_snapshot(UUID, SnapID);
                                false ->
                                    ok
                            end;
                        {error, E} ->
                            fifo_s3:abort_upload(Upload),
                            lager:error("Upload error: ~p", [E]),
                            libsniffle:vm_set(
                              UUID, [<<"snapshots">>, SnapID, <<"state">>],
                              <<"upload failed">>),
                            ok
                    end
            end;
        {Port, {exit_status, S}} ->
            fifo_s3:abort_upload(Upload),
            lager:error("Upload error: ~p", [S]),
            libsniffle:vm_set(
              UUID, [<<"snapshots">>, SnapID, <<"state">>],
              <<"upload failed">>),
            ok
    end.


download_snapshot(UUID, SnapID, Host, Port, Bucket, AKey, SKey, Bucket, _Options) ->
    Conf = fifo_s3:make_config(AKey, SKey, Host, Port),
    {ok, Download} = fifo_s3:new_stream(Bucket, SnapID, Conf,
                                        [{chunk_size, 10485760}]),
    Cmd = code:priv_dir(chunter) ++ "/zfs_import.gzip.sh",
    lager:debug("Running ZFS command: ~p ~s ~s", [Cmd, UUID, SnapID]),
    Prt = open_port({spawn_executable, Cmd},
                    [{args, [UUID, SnapID]}, use_stdio, binary,
                     stderr_to_stdout, exit_status, stream]),
    libsniffle:vm_set(
      UUID, [<<"snapshots">>, SnapID, <<"state">>], <<"downloading">>),
    download_snapshot(Prt, Download, 0).

download_snapshot(Prt, Download, I) ->
    case fifo_s3:get_stream(Download) of
        {ok, Data, Download1} ->
            lager:debug("Download part: ~p.", [I]),
            port_command(Prt, Data),
            download_snapshot(Prt, Download1, I+1);
        {error, E} ->
            port_close(Prt),
            lager:error("Import error: ~p", [I, E]),
            ok;
        {ok, done} ->
            lager:debug("Download complete: ~p.", [I]),
            port_close(Prt),
            ok
    end.
