%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_vmadm).

%% API
-export([start/1,
         start/2,
         stop/1,
         force_stop/1,
         info/1,
         reboot/1,
         force_reboot/1,
         delete/1,
         create/1,
         update/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

zoneadm(UUID, uninstall) ->
    zoneadmf(UUID, <<"uninstall">>);
zoneadm(UUID, SubCmd) when is_atom(SubCmd) ->
    zoneadm(UUID, atom_to_list(SubCmd));
zoneadm(UUID, SubCmd) when is_list(SubCmd) ->
    zoneadm(UUID, list_to_binary(SubCmd));

zoneadm(UUID, SubCmd) ->
    Cmd = <<"/usr/sbin/zoneadm -z ", UUID/binary, " ", SubCmd/binary>>,
    lager:debug([{fifi_component, chunter}],
                "zoneadm:cmd - ~s.", [Cmd]),
    R = os:cmd(binary_to_list(Cmd)),
    lager:debug("[zoneadm] ~s", [R]),
    R.

zoneadmf(UUID, SubCmd) ->
    Cmd = <<"/usr/sbin/zoneadm -z ", UUID/binary, " ", SubCmd/binary, " -F">>,
    lager:debug([{fifi_component, chunter}],
                "zoneadm:cmd - ~s.", [Cmd]),
    R = os:cmd(binary_to_list(Cmd)),
    lager:debug("[zoneadm] ~s", [R]),
    R.

zonecfg(UUID, delete) ->
    zonecfgf(UUID, <<"delete">>);
zonecfg(UUID, SubCmd) when is_atom(SubCmd) ->
    zonecfg(UUID, atom_to_list(SubCmd));
zonecfg(UUID, SubCmd) when is_list(SubCmd) ->
    zonecfg(UUID, list_to_binary(SubCmd));

zonecfg(UUID, SubCmd) ->
    Cmd = <<"/usr/sbin/zonecfg -z ", UUID/binary, " ", SubCmd/binary>>,
    lager:debug([{fifi_component, chunter}],
                "zonecfg:cmd - ~s.", [Cmd]),
    R = os:cmd(binary_to_list(Cmd)),
    lager:debug("[zonecfg] ~s", [R]),
    R.

zonecfgf(UUID, SubCmd) ->
    Cmd = <<"/usr/sbin/zonecfg -z ", UUID/binary, " ", SubCmd/binary, " -F">>,
    lager:debug([{fifi_component, chunter}],
                "zonecfg:cmd - ~s.", [Cmd]),
    R = os:cmd(binary_to_list(Cmd)),
    lager:debug("[zonecfg] ~s", [R]),
    R.




%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

-spec start(UUID::fifo:uuid()) -> list().
start(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info([{fifi_component, chunter}],
                       "vmadm:start - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm start ", UUID/binary>>,
            lager:debug([{fifi_component, chunter}],
                        "vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        omnios ->
            lager:info([{fifi_component, chunter}],
                       "zoneadm:start - UUID: ~s.", [UUID]),
            zoneadm(UUID, boot)
    end.

-spec start(UUID::fifo:uuid(), Image::binary()) -> list().

start(UUID, Image) ->
    case chunter_utils:system() of
        smartos ->
            lager:info([{fifi_component, chunter}],
                        "vmadm:start - UUID: ~s, Image: ~s.", [UUID, Image]),
            Cmd = <<"/usr/sbin/vmadm start ", UUID/binary>>,
            lager:debug([{fifi_component, chunter}],
                        "vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        omnios ->
            lager:inafo([{fifi_component, chunter}],
                        "zoneadm:start - UUID: ~s, Image: ~s.", [UUID, Image]),
            zoneadm(UUID, boot)
    end.

-spec delete(UUID::fifo:uuid()) -> ok.

delete(UUID) ->
    R1 = case chunter_utils:system() of
             smartos ->
                 lager:info([{fifi_component, chunter}],
                            "vmadm:delete - UUID: ~s.", [UUID]),
                 Cmd = <<"/usr/sbin/vmadm delete ", UUID/binary>>,
                 lager:debug([{fifi_component, chunter}],
                             "vmadm:cmd - ~s.", [Cmd]),
                 R = os:cmd(binary_to_list(Cmd)),
                 lager:debug("[vmadm] ~s", [R]),
                 R;
             omnios ->
                 VM = chunter_zone:get(UUID),
                 force_stop(UUID),
                 lager:info([{fifi_component, chunter}],
                            "zoneadm:uninstall - UUID: ~s / ~p.", [UUID, VM]),
                 zoneadm(UUID, uninstall),
                 zonecfg(UUID, delete),
                 {ok, Nics} = jsxd:get(<<"nics">>, VM),
                 lists:map(fun(N) ->
                                   {ok, IFace} = jsxd:get(<<"interface">>, N),
                                   chunter_nic_srv:delete(IFace)
                           end, Nics)
         end,
    chunter_server:update_mem(),
    R1.

-spec info(UUID::fifo:uuid()) -> fifo:config_list().

info(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info([{fifi_component, chunter}],
                       "vmadm:info - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm info ", UUID/binary>>,
            lager:debug([{fifi_component, chunter}],
                        "vmadm:cmd - ~s.", [Cmd]),
            case os:cmd(binary_to_list(Cmd)) of
                "Unable" ++ _ ->
                    {error, no_info};
                JSON ->
                    case jsx:to_term(list_to_binary(JSON)) of
                        {incomplete, _} ->
                            {error, no_info};
                        R ->
                            {ok, R}
                    end
            end;

        omnios ->
            {error, no_info}
    end.

-spec stop(UUID::fifo:uuid()) -> list().

stop(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info([{fifi_component, chunter}],
                       "vmadm:stop - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm stop ", UUID/binary>>,
            lager:debug([{fifi_component, chunter}],
                        "vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        omnios ->
            lager:info([{fifi_component, chunter}],
                       "zoneadm:stop - UUID: ~s.", [UUID]),
            zoneadm(UUID, shutdown)
    end.

-spec force_stop(UUID::fifo:uuid()) -> list().

force_stop(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info([{fifi_component, chunter}],
                       "vmadm:force-stop - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm stop ", UUID/binary, " -F">>,
            lager:debug([{fifi_component, chunter}],
                        "vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        omnios ->
            lager:info([{fifi_component, chunter}],
                       "vmadm:force-stop - UUID: ~s.", [UUID]),
            zoneadm(UUID, halt)
    end.

-spec reboot(UUID::fifo:uuid()) -> list().

reboot(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info([{fifi_component, chunter}],
                       "vmadm:reboot - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm reboot ", UUID/binary>>,
            lager:debug([{fifi_component, chunter}],
                        "vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        omnios ->
            lager:info([{fifi_component, chunter}],
                       "vmadm:reboot - UUID: ~s.", [UUID]),
            zoneadm(UUID, "shutdown -r")
    end.


-spec force_reboot(UUID::fifo:uuid()) -> list().

force_reboot(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info([{fifi_component, chunter}],
                       "vmadm:reboot - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm reboot ", UUID/binary, " -F">>,
            lager:debug([{fifi_component, chunter}],
                        "vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        omnios ->
            lager:info([{fifi_component, chunter}],
                       "vmadm:reboot - UUID: ~s.", [UUID]),
            zoneadm(UUID, reboot)
    end.

-spec create(UUID::fifo:vm_config()) -> ok |
                                        {error, binary() |
                                         timeout |
                                         unknown}.

create(Data) ->
    lager:info("New Create: ~p", [Data]),
    {<<"uuid">>, UUID} = lists:keyfind(<<"uuid">>, 1, Data),
    lager:info("Creation of VM '~s' started.", [UUID]),
    lager:info([{fifi_component, chunter}],
               "vmadm:create", []),
    Cmd = "/usr/sbin/vmadm create",
    lager:debug([{fifi_component, chunter}],
                "vmadm:cmd - ~s.", [Cmd]),
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000},
                                    stderr_to_stdout, exit_status]),
    port_command(Port, jsx:to_json(Data)),
    lager:info([{fifi_component, chunter}],
               "vmadm:create - handed to vmadm, waiting ...", []),
    timer:apply_after(5000, chunter_server, update_mem, []),
    Res = case wait_for_text(Port, UUID, 60*10) of
              ok ->
                  lager:info([{fifi_component, chunter}],
                             "vmadm:create - vmadm returned sucessfully.", []),
                  libhowl:send(<<"command">>,
                               [{<<"event">>, <<"vm-create">>},
                                {<<"uuid">>, uuid:uuid4s()},
                                {<<"data">>,
                                 [{<<"uuid">>, UUID}]}]),
                  chunter_vm_fsm:load(UUID);
              {error, E} ->
                  delete(UUID),
                  lager:error([{fifi_component, chunter}],
                              "vmad:create - Failed: ~p.", [E]),
                  {error, E}
          end,
    lager:info([{fifi_component, chunter}],
               "vmadm:create - updating memory.", []),
    chunter_server:update_mem(),
    ls_vm:creating(UUID, false),
    Res.

update(UUID, Data) ->
    lager:info("~p", [<<"Updating of VM '", UUID/binary, "' started.">>]),
    %%    libsnarl:msg(Owner, info, <<"Creation of VM '", Alias/binary, "' started.">>),
    lager:info([{fifi_component, chunter}],
               "vmadm:update", []),
    Cmd =  code:priv_dir(chunter) ++ "/vmadm_wrap.sh update " ++ binary_to_list(UUID),
    lager:debug([{fifi_component, chunter}],
                "vmadm:cmd - ~s.", [Cmd]),
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000}, stderr_to_stdout, exit_status]),
    port_command(Port, jsx:to_json(Data)),
    port_command(Port, "\nEOF\n"),
    receive
        {Port, {data, {eol, Data}}} ->
            lager:debug("[vmadm] ~s", [Data]);
        {Port, {data, Data}} ->
            lager:debug("[vmadm] ~s", [Data]);
        {Port, {exit_status, 0}} ->
            chunter_server:update_mem(),
            chunter_vm_fsm:load(UUID);
        {Port, {exit_status, E}} ->
            chunter_server:update_mem(),
            chunter_vm_fsm:load(UUID),
            {error, E}
    after
        60000 ->
            chunter_server:update_mem(),
            chunter_vm_fsm:load(UUID)
    end.


%% This function reads the process's input untill it knows that the vm was created or failed.
%%-spec wait_for_text(Port::any()) ->
%%                           {ok, UUID::fifo:uuid()} |
%%                           {error, Text::binary() |
%%                                         timeout |
%%                                         unknown}.
%%wait_for_text(Port) ->
%%    wait_for_text(Port, undefined).

%%wait_for_text(Port, Lock) ->
%%    wait_for_text(Port, Lock, 60*10).

wait_for_text(Port, Lock, Max) ->
    wait_for_text(Port, Lock, Max, now()).

wait_for_text(Port, Lock, Max, Timeout) ->
    receive
        {Port, {data, {eol, Data}}} ->
            lager:debug("[vmadm] ~s", [Data]),
            relock(Lock),
            wait_for_text(Port, Lock, Max, Timeout);
        {Port, {data, Data}} ->
            lager:debug("[vmadm] ~s", [Data]),
            relock(Lock),
            wait_for_text(Port, Lock, Max, Timeout);
        {Port,{exit_status, 0}} ->
            ok;
        {Port,{exit_status, S}} ->
            {error, S}
    after
        3000 ->
            case timer:now_diff(now(), Timeout) of
                _To when _To  > (Max*1000000) ->
                    lager:debug("[vmadm] timeout after ~ps", [Max]);
                _ ->
                    relock(Lock),
                    wait_for_text(Port, Lock, Max, Timeout)
            end
    end.

relock(undefined) ->
    ok;
relock(Lock) ->
    chunter_lock:lock(Lock).



%%%===================================================================
%%% Internal functions
%%%===================================================================
