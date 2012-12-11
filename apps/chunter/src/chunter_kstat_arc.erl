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

-record(state, {host}).

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
handle_info(tick, State) ->
    Values = get_stats("/usr/bin/kstat -p zfs:0:arcstats"),
    {_, L1Hit} = lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"hits">>}, 1, Values),
    {_, L1Miss} = lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"misses">>}, 1, Values),
    {_, L1Size} = lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"size">>}, 1, Values),

    {_, L2Hit} = lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"l2_hits">>}, 1, Values),
    {_, L2Miss} = lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"l2_misses">>}, 1, Values),
    {_, L2Size} = lists:keyfind({<<"zfs">>,<<"0">>,<<"arcstats">>,<<"l2_size">>}, 1, Values),

    libsniffle:hypervisor_resource_set(State#state.host,
				       [{<<"l2hits">>, L2Hit},
					{<<"l2miss">>, L2Miss},
					{<<"l2size">>, round(L2Size/(1024*1024))},
					{<<"l1hits">>, L1Hit},
					{<<"l1miss">>, L1Miss},
					{<<"l1size">>, round(L1Size/(1024*1024))}
				       ]),

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
