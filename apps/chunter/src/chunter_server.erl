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

-include("chunter_version.hrl").

%% API
-export([start_link/0,
         host_info/0,
         connect/0,
         update_mem/0,
         reserve_mem/1,
         disconnect/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0]).

-define(CPU_CAP_MULTIPLYER, 8).

-define(SERVER, ?MODULE).

-define(HOST_ID_FILE, "host_id").

-record(state, {name,
                port,
                sysinfo,
                connected = false,
                capabilities = [],
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

connect() ->
    gen_server:cast(?SERVER, connect).

update_mem() ->
    gen_server:cast(?SERVER, update_mem).

reserve_mem(N) ->
    gen_server:cast(?SERVER, {reserve_mem, N}).

disconnect() ->
    gen_server:cast(?SERVER, disconnect).

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
    %% We subscribe to sniffle register channel - that way we can reregister to dead sniffle processes.
    mdns_client_lib_connection_event:add_handler(chunter_connect_event),
    register_hypervisor(),
    lists:foldl(
      fun (VM, _) ->
              {<<"uuid">>, UUID} = lists:keyfind(<<"uuid">>, 1, VM),
              chunter_vm_fsm:load(UUID)
      end, 0, list_vms()),

    SysInfo = jsx:decode(list_to_binary(os:cmd("sysinfo"))),


    Capabilities = case os:cmd("ls /dev/kvm") of
                       "/dev/kvm\n" ->
                           [<<"zone">>, <<"kvm">>];
                       _ ->
                           [<<"zone">>]
                   end,
    {Host, _IPStr} = host_info(),
    libsniffle:hypervisor_set(Host, [{<<"sysinfo">>, SysInfo},
                                     {<<"version">>, ?VERSION},
                                     {<<"virtualisation">>, Capabilities}]),

    {ok, #state{
            sysinfo = SysInfo,
            name = Host,
            capabilities = Capabilities
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

handle_call({call, _Auth, Call}, _From, #state{name = _Name} = State) ->
    %%    statsderl:increment([Name, ".call.unknown"], 1, 1.0),
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

handle_cast(update_mem, State = #state{name = Host}) ->
    VMS = list_vms(),
    ProvMem = round(lists:foldl(
                      fun (VM, Mem) ->
                              {<<"max_physical_memory">>, M} = lists:keyfind(<<"max_physical_memory">>, 1, VM),
                              Mem + M
                      end, 0, VMS) / (1024*1024)),
    {TotalMem, _} =
        string:to_integer(
          os:cmd("/usr/sbin/prtconf | grep Memor | awk '{print $3}'")),
    lager:info("[~p] Counting ~p MB used out of ~p MB in total.",
               [Host, ProvMem, TotalMem]),
    libsniffle:hypervisor_set(Host, [{<<"resources.free-memory">>, TotalMem - ProvMem},
                                     {<<"resources.provisioned-memory">>, ProvMem},
                                     {<<"resources.total-memory">>, TotalMem}]),
    {noreply, State#state{
                total_memory = TotalMem,
                provisioned_memory = ProvMem
               }};

handle_cast({reserve_mem, N}, State =
                #state{
                   name = Host,
                   total_memory = TotalMem,
                   provisioned_memory = ProvMem
                  }) ->
    ProvMem1 = ProvMem + N,
    Free = TotalMem - ProvMem1,
    libsniffle:hypervisor_set(Host,
                              [{<<"resources.free-memory">>, Free},
                               {<<"resources.provisioned-memory">>, ProvMem1}]),
    {noreply, State#state{
                provisioned_memory = ProvMem1
               }};

handle_cast(connect, #state{name = Host,
                            capabilities = Caps} = State) ->
    %%    {ok, Host} = libsnarl:option_get(system, statsd, hostname),
    %%    application:set_env(s59tatsderl, hostname, Host),
    {TotalMem, _} = string:to_integer(os:cmd("/usr/sbin/prtconf | grep Memor | awk '{print $3}'")),
    Networks = re:split(os:cmd("cat /usbkey/config  | grep -v '^#' | grep '_nic=' | sed 's/_nic.*$//'"), "\n"),
    Networks1 = lists:delete(<<>>, Networks),
    Etherstub = re:split(
                  os:cmd("cat /usbkey/config | grep etherstub | sed -e 's/etherstub=\"\\(.*\\)\"/\\1/'"),
                  ",\\s*|\n"),
    Etherstub1 = lists:delete(<<>>, Etherstub),
    VMS = list_vms(),
    ProvMem = round(lists:foldl(
                      fun (VM, Mem) ->
                              {<<"uuid">>, UUID} = lists:keyfind(<<"uuid">>, 1, VM),
                              chunter_vm_fsm:load(UUID),
                              {<<"max_physical_memory">>, M} = lists:keyfind(<<"max_physical_memory">>, 1, VM),
                              Mem + M
                      end, 0, VMS) / (1024*1024)),

    %%    statsderl:gauge([Name, ".hypervisor.memory.total"], TotalMem, 1),
    %%    statsderl:gauge([Name, ".hypervisor.memory.provisioned"], ProvMem, 1),

    %%    statsderl:increment([Name, ".net.join"], 1, 1.0),
    %%    libsniffle:join_client_channel(),

    register_hypervisor(),
    libsniffle:hypervisor_set(
      Host,
      [{<<"sysinfo">>, State#state.sysinfo},
       {<<"version">>, ?VERSION},
       {<<"networks">>, Networks1},
       {<<"resources.free-memory">>, TotalMem - ProvMem},
       {<<"etherstubs">>, Etherstub1},
       {<<"resources.provisioned-memory">>, ProvMem},
       {<<"resources.total-memory">>, TotalMem},
       {<<"virtualisation">>, Caps}]),
    {noreply, State#state{
                total_memory = TotalMem,
                provisioned_memory = ProvMem,
                connected = true
               }};

handle_cast(disconnect,  State) ->
    {noreply, State#state{connected = false}};


handle_cast(Msg, #state{name = Name} = State) ->
    lager:warning("[~p] unknown message: ~p.", [Name, Msg]),
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

list_vms() ->
    [chunter_zoneparser:load([{<<"name">>, Name}, {<<"uuid">>, UUID}]) ||
        [ID,Name,_VMState,_Path,UUID,_Type,_IP,_SomeNumber] <-
            [ re:split(Line, ":")
              || Line <- re:split(os:cmd("/usr/sbin/zoneadm list -ip"), "\n")],
        ID =/= <<"0">>].


host_info() ->
    Path = [code:root_dir(), "/etc/" ?HOST_ID_FILE],
    Host = case application:get_env(chunter, hostname) of
               undefined ->
                   [H|_] = re:split(os:cmd("uname -n"), "\n"),
                   H;
               {ok, H} when is_binary(H) ->
                   H;
               {ok, H} when is_list(H) ->
                   list_to_binary(H)
           end,
    {A,B,C,D} = case application:get_env(chunter, ip) of
                    undefined ->
                        {ok, R} = inet:getaddr(binary_to_list(Host), inet),
                        R;
                    {ok, R} ->
                        R
                end,
    IPStr = list_to_binary(io_lib:format("~p.~p.~p.~p", [A,B,C,D])),
    HostID = case filelib:is_file([code:root_dir(), "/etc/host_id"]) of
                 true ->
                     list_to_binary(os:cmd(["cat ", Path]));
                 _ ->
                     UUID = uuid:uuid4s(),
                     file:write_file(Path, UUID),
                     UUID
             end,

    {HostID, IPStr}.


register_hypervisor() ->
    {Host, IPStr} = host_info(),
    lager:info([{fifi_component, chunter}],
               "chunter:init - Host: ~s(~s)", [Host, IPStr]),
    [Alias|_] = re:split(os:cmd("uname -n"), "\n"),
    case libsniffle:hypervisor_get(Host) of
        not_found ->
            libsniffle:hypervisor_register(Host, IPStr, 4200),
            libsniffle:hypervisor_set(Host, <<"alias">>, Alias);
        {ok, H} ->
            libsniffle:hypervisor_register(Host, IPStr, 4200),
            case jsxd:get(<<"alias">>, H) of
                undefined ->
                    libsniffle:hypervisor_set(Host, <<"alias">>, Alias);
                _ ->
                    ok
            end;
        _ ->
            libsniffle:hypervisor_register(Host, IPStr, 4200),
            ok
    end.
