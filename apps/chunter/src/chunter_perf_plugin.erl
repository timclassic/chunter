%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 14 Mar 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_perf_plugin).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0]).

-define(SERVER, ?MODULE).

-record(state, {i=0, kstat}).

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
    eplugin:call('perf:init'),
    {ok, Handle} = ekstat:open(),
    {ok, #state{kstat = Handle}}.

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
handle_info(tick, State = #state{kstat = KStat, i = I}) when (I rem 30) =:=0->
    ekstat:update(KStat),
    Res = lists:foldl(fun merge/2, [], eplugin:call('perf:tick:30s', KStat)),
    Res1 = lists:foldl(fun merge/2, Res, eplugin:call('perf:tick:10s', KStat)),
    Res2 = lists:foldl(fun merge/2, Res1, eplugin:call('perf:tick:1s', KStat)),
    Res3 = lists:map(fun ({K, V}) ->
                             {<<K/binary, "-metrics">>, V}
                     end, Res2),
    libhowl:send(Res3),
    {noreply, State};

handle_info(tick, State = #state{kstat = KStat, i = I}) when (I rem 10) =:=0->
    ekstat:update(KStat),
    Res = lists:foldl(fun merge/2, [], eplugin:call('perf:tick:10s', KStat)),
    Res1 = lists:foldl(fun merge/2, Res, eplugin:call('perf:tick:1s', KStat)),
    Res2 = lists:map(fun ({K, V}) ->
                             {<<K/binary, "-metrics">>, V}
                     end, Res1),
    libhowl:send(Res2),
    {noreply, State};
handle_info(tick, State = #state{kstat = KStat, i = I}) when (I rem 10) =:=0->
    ekstat:update(KStat),
    Res = lists:foldl(fun merge/2, [], eplugin:call('perf:tick:1s', KStat)),
    Res1 = lists:map(fun ({K, V}) ->
                             {<<K/binary, "-metrics">>, V}
                     end, Res),
    libhowl:send(Res1),
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

merge(A, B) ->
    jsxd:merge(fun merge_fn/3, A, B).

merge_fn(_, [{_,_} |_] = A, B) when is_list(A), is_list(B) ->
    jsxd:merge(fun merge_fn/3, A, B);

merge_fn(_, A, [{_,_} |_] = B) when is_list(A), is_list(B) ->
    jsxd:merge(fun merge_fn/3, A, B);

merge_fn(_, A, B) when is_list(A), is_list(B) ->
    A ++ B;

merge_fn(_, A, B) when is_number(A), is_number(B) ->
    A + B.
