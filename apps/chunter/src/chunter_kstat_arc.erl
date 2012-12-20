%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 11 Dec 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_kstat_arc).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ignore_xref([start_link/0]).


-define(SERVER, ?MODULE).

-record(state, {host,
		l1hit, l1miss, l1size,
		l2hit, l2miss, l2size,
		skipped = 0}).

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
    timer:send_interval(1000, tick),
    [Host|_] = re:split(os:cmd("uname -n"), "\n"),
    {ok, #state{host = Host}}.

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
handle_info(tick, State = #state{
		    l1hit = L1HitOld, l1miss = L1MissOld, l1size = L1SizeOld,
		    l2hit = L2HitOld, l2miss = L2MissOld, l2size = L2SizeOld,
		    skipped = Skipped
		   }) ->
    Values = get_stats("/usr/bin/kstat -p zfs:0:arcstats"),
    {State0, V0} = case lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"hits">>}, 1, Values) of
		       {_, L1HitOld} when Skipped < 120 -> {State#state{skipped = Skipped+1}, []};
		       {_, L1Hit} -> {State#state{l1hit = L1Hit}, [{<<"l1hits">>, L1Hit}]}
		   end,
    {State1, V1} = case lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"misses">>}, 1, Values) of
		       {_, L1MissOld} when Skipped < 120 -> {State0#state{skipped = Skipped+1}, V0};
		       {_, L1Miss} -> {State0#state{l1miss = L1Miss}, [{<<"l1miss">>, L1Miss} | V0]}
		   end,
    {State2, V2} = case lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"size">>}, 1, Values) of
		       {_, L1SizeOld} when Skipped < 120 -> {State1#state{skipped = Skipped+1}, V1};
		       {_, L1Size} -> {State1#state{l1size = L1Size}, [{<<"l1size">>, round(L1Size/(1024*1024))} | V1]}
		   end,

    {State3, V3} = case lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"l2_hits">>}, 1, Values) of
		       {_, L2HitOld} when Skipped < 120 -> {State2#state{skipped = Skipped+1}, V2};
		       {_, L2Hit} -> {State2#state{l2hit = L2Hit}, [{<<"l2hits">>, L2Hit} | V2]}
		   end,
    {State4, V4} = case lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"l2_misses">>}, 1, Values) of
		       {_, L2MissOld} when Skipped < 120 -> {State3#state{skipped = Skipped+1}, V3};
		       {_, L2Miss} -> {State3#state{l2miss = L2Miss}, [{<<"l2miss">>, L2Miss} | V3]}
		   end,
    {State5, V5} = case lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"l2_size">>}, 1, Values) of
		       {_, L2SizeOld} when Skipped < 120-> {State4#state{skipped = Skipped+1}, V4};
		       {_, L2Size} -> {State4#state{l2size = L2Size}, [{<<"l2size">>, round(L2Size/(1024*1024))} | V4]}
		   end,


    case V5 of
	[] ->
    	    {noreply, State5};
	_ ->
	    libsniffle:hypervisor_resource_set(State5#state.host, V5),
	    {noreply, State5#state{skipped = 0}}
    end;

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


get_stats(Cmd) ->
    lists:map(fun (Line) ->
		      [Ks, Vs] = re:split(Line, "\t"),
		      [A,B,C,D] = re:split(Ks, ":"),
		      case re:run(Vs, "^\\d+$") of
			  nomatch ->
			      {{A,B,C,D}, Vs};
			  _ ->
			      {{A,B,C,D}, list_to_integer(binary_to_list(Vs))}
		      end
	      end, re:split(os:cmd(Cmd), "\n", [trim])).
