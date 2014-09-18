%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@schroedinger.local>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2014 by Heinz Nikolaus Gies <heinz@schroedinger.local>
%%%-------------------------------------------------------------------
-module(ezdoor_server).

-behaviour(gen_server).

%% API
-export([start_link/0, add/3, remove/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HEATRBEAT_INTERVAL, 1000). % 2 seconds - zonedoor terminates after 7 sec of no HB. Allow 2 misses
-define(LINE_WIDTH, 2048). % 2 seconds - zonedoor terminates after 7 sec of no HB. Allow 2 misses


-record(door, {ref, zone, door, pid, monitor, module}).
-record(state, {port, doors}).

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

add(Module, ZoneUUID, DoorName) ->
    gen_server:call(?SERVER, {add, Module, ZoneUUID, DoorName}).

remove(Ref) ->
    gen_server:call(?SERVER, {remove, Ref}).

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
    Cmd = code:priv_dir(chunter) ++ "/zonedoor",
    PortOpts = [{args, []}, use_stdio, {line, ?LINE_WIDTH}, exit_status],
    DoorPort = open_port({spawn_executable, Cmd},  PortOpts),
    erlang:send_after(?HEATRBEAT_INTERVAL, self(), heartbeat),
    process_flag(trap_exit, true),
    {ok, #state{port = DoorPort}}.

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
handle_call({add, Module, ZoneUUID, DoorName}, From,
            State = #state{port = Port, doors = Doors}) ->
    case zdoor_exists(ZoneUUID, DoorName, Doors) of
        true ->
            {reply, {error, doublicate}, State};
        false ->
            Ref = make_ref(),
            port_command(Port, [$a, ZoneUUID, $\s, DoorName, $\s, r2s(Ref), $\n]),
            MRef = do_monitor(From, Doors),
            Door = #door{ref=Ref, zone=ZoneUUID, door=DoorName, pid=From,
                         monitor=MRef, module=Module},
            {reply, {ok, Ref}, State#state{doors = [Door | Doors]}}
    end;

handle_call({remove, Ref}, From,
            State = #state{port = Port, doors = Doors}) ->
    case find_door(Ref, Doors) of
        {ok, D = #door{zone=UUID, door=Door}} ->
            port_command(Port, [$d, UUID, $\s, Door, $\n]),
            Doors1 = lists:delete(D, Doors),
            case find_monitor(From, Doors1) of
                undefined ->
                    demonitor(Door#door.monitor);
                _ ->
                    ok
            end,
            {reply, ok, State#state{doors = Doors1}};
        _ ->
            {reply, ok, State}
    end;

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
handle_info(heartbeat, State) ->
    port_command(State#state.port, "h\n"),
    erlang:send_after(?HEATRBEAT_INTERVAL, self(), heartbeat),
    {noreply, State};

handle_info({'DOWN', MRef, _Type, _Object, _Info},
            State = #state{doors = Doors, port = Port}) ->
    Doors1 = [D || D<-Doors, D#door.monitor /= MRef],
    [port_command(Port, [$d, Zone, $\s, Door, $\n])
     || #door{zone=Zone, door=Door, monitor=AMRef}<-Doors, AMRef == MRef],
    {noreply, State#state{doors = Doors1}};

handle_info({Port,{data, {eol,Data}}},
            State = #state{port = Port, doors=Doors}) ->
    {Ref, Cmd} = extract_ref(Data),
    case find_door(Ref, Doors) of
        {ok, #door{pid=Pid, module=Mod}} ->
            {ok, Res} = Mod:door_event(Pid, Ref, Cmd),
            port_command(Port, [$r, Res]);
        _ ->
            ok
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
terminate(_Reason, #state{port = Port, doors = Doors}) ->
    [port_command(Port, [$d, Zone, $\s, Door, $\n])
     || #door{zone=Zone, door=Door}<-Doors],
    incinerate(Port),
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

r2s(Ref) ->
    base64:encode(term_to_binary(Ref)).

s2r(Str) ->
    binary_to_term(base64:decode(Str)).

zdoor_exists(_UUID, _Name, []) ->
    false;
zdoor_exists(UUID, Name, [#door{zone=UUID, door=Name} | _]) ->
    true;
zdoor_exists(UUID, Name, [_ | R]) ->
    zdoor_exists(UUID, Name, R).

find_door(_Ref, []) ->
    undefined;
find_door(Ref, [D=#door{ref=Ref} | _]) ->
    {ok, D};
find_door(Ref, [_ | R]) ->
    find_door(Ref, R).

find_monitor(_From, []) ->
    undefined;
find_monitor(From, [D=#door{pid=From} | _]) ->
    {ok, D};
find_monitor(From, [_ | R]) ->
    find_monitor(From, R).

do_monitor(From, Doors) ->
    case find_monitor(From, Doors) of
        undefined ->
            monitor(process, From);
        {ok, D} ->
            D#door.monitor
    end.

extract_ref(Data) ->
    extract_ref(Data, <<>>).

extract_ref(<<$\s, Data/binary>>, Ref)->
    {s2r(Ref), Data};

extract_ref(<<C, Data/binary>>, Ref) ->
    extract_ref(Data, <<Ref/binary, C>>).

incinerate(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            port_close(Port),
            lager:warning("Killing ~p with -9", [OsPid]),
            os:cmd(io_lib:format("/usr/bin/kill -9 ~p", [OsPid]));
        undefined ->
            lager:warning("Process has no pid not incinerating.")
    end.
