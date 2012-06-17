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

-record(state, {name, zoneport, statport, statspec=[]}).

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
    Cmd = code:priv_dir(chunter) ++ "/zonemon.d",
    ZonePort = erlang:open_port({spawn, Cmd},[exit_status, use_stdio, binary, {line, 1000}]),
    StatPort = erlang:open_port({spawn, "vmstat 5"},[exit_status, use_stdio, binary, {line, 1000}]),
    {ok, #state{
       name=Name,
       zoneport=ZonePort,
       statport=StatPort}}.

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
handle_info({_Port, {data, {eol, Data}}}, #state{statport=_Port, statspec=Spec, name=Name} = State) ->
    case parse_stat(Data, Spec) of
	skip ->
	    {noreply, State};
	{spec, NewSpec} ->
	    {noreply, State#state{statspec=NewSpec}};
	{stat, Stat} ->
	    io:format(Stat),
	    gproc:send({p,g,{node,Name}}, {stat, Stat}),
	    {noreply, State}
    end;
handle_info({_Port, {data, {eol, Data}}}, #state{zoneport=_Port} = State) ->
    case parse_data(Data) of
	{error, unknown} ->
	    io:format("Data: ~p~n", [Data]);
	{UUID, crate} ->
	    chunter_vm_sup:start_child(UUID);
	{UUID, Action} ->
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
terminate(_Reason, #state{statport=SPort,
			  zoneport=Zport}) ->
    erlang:port_close(SPort),
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

parse_stat(<<" k", _/binary>>, _) ->
    skip;
parse_stat(<<" r ", Specs/binary>>, _) ->
    {spec, [<<"r">> | re:split(Specs, "\s+")]};
parse_stat(<<" ", Data/binary>>, Specs) ->
    Res = re:split(Data, "\s+"),
    {stat, build_stat(Specs, Res)}.

build_stat(S, D) ->
    build_stat(S, D, kthr, [], [], [], [], [], []).

build_stat([], _, _Cat, K, M, P, D, F, C) ->
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
    build_stat(R, RV, cpu, K, M, P, D, [{system_calls, V}|F], C);

build_stat([<<"us">>|R], [V|RV], cpu, K, M, P, D, F, C) ->
    build_stat(R, RV, cpu, K, M, P, D, F, [{user, V}|C]);
build_stat([<<"sy">>|R], [V|RV], cpu, K, M, P, D, F, C) ->
    build_stat(R, RV, cpu, K, M, P, D, F, [{system, V}|C]);
build_stat([<<"id">>|R], [V|RV], cpu, K, M, P, D, F, C) ->
    build_stat(R, RV, cpu, K, M, P, D, F, [{idel, V}|C]).




