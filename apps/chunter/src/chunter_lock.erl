%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 25 Feb 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_lock).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0,
         lock/1,
         release/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0]).

-define(SERVER, ?MODULE).

%% Timeout for which a lock can be held before it needs to release.
-ifndef(TEST).
%% Default is 1m.
-define(LOCK_TIMEOUT, 60*1000).
-else.
%% In tests we want this to be much shorter.
-define(LOCK_TIMEOUT, 500).
-endif.


-record(state, {lock=undefined}).

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

lock(UUID) ->
    gen_server:call(?SERVER, {lock, UUID}, 500).

release(UUID) ->
    gen_server:call(?SERVER, {release, UUID}, 500).

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
    {ok, #state{}}.

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
handle_call({release, UUID}, _From, #state{lock = undefined} = State) ->
    lager:warning("[lock] Tried to release ~s we were not locked.",
                  [UUID]),
    {reply, failed, State};

handle_call({release, UUID}, _From, #state{lock = {UUID, Old}} = State) ->
    D = erlang:system_time(milli_seconds) - Old,
    lager:info("[lock] Lock ~s released after ~ps", [UUID, D/1000]),
    {reply, ok, State#state{lock=undefined}};

handle_call({release, NewUUID}, _From, #state{lock = {OldUUID, _}} = State) ->
    lager:warning("[lock] Tried to release ~s but ~s was the lock.",
                  [NewUUID, OldUUID]),
    {reply, failed, State};

handle_call({lock, UUID}, _From, #state{lock = undefined} = State) ->
    lager:info("[lock] Lock ~s claimed", [UUID]),
    {reply, ok, State#state{lock = {UUID, erlang:system_time(milli_seconds)}}};

handle_call({lock, UUID}, _From, #state{lock = {UUID, Old}} = State) ->
    D = erlang:system_time(milli_seconds) - Old,
    lager:info("[lock] Lock ~s renewed after ~ps", [UUID, D/1000]),
    {reply, ok, State#state{lock = {UUID, erlang:system_time(milli_seconds)}}};

handle_call({lock, NewUUID}, _From, #state{lock = {OldUUID, Old}} = State) ->
    case erlang:system_time(milli_seconds) - Old of
        D when D > ?LOCK_TIMEOUT ->
            io:format("~p > ~p ~n.", [D, ?LOCK_TIMEOUT]),
            lager:warning("[lock] Lock ~s timed out after ~ps and replaced by "
                          "~s.", [OldUUID, D/1000, NewUUID]),
            {reply, ok, State#state{lock = {NewUUID, erlang:system_time(milli_seconds)}}};
        D ->
            lager:info("[lock] Lock ~s rejected old lock ~s still in effect "
                       "for another ~ps.",
                       [NewUUID, OldUUID, (?LOCK_TIMEOUT - D)/1000]),
            {reply, failed, State}
    end.

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


-ifdef(TEST).

lock_test() ->
    State0 = #state{},
    {reply, R1, State1} = handle_call({lock, 1}, from, State0),
    {reply, R2, State2} = handle_call({lock, 2}, from, State1),
    {reply, R3, State3} = handle_call({lock, 2}, from, State2),
    {reply, R4, State4} = handle_call({release, 2}, from, State3),
    {reply, R5, State5} = handle_call({release, 1}, from, State4),
    {reply, R6, _State6} = handle_call({release, 1}, from, State5),
    ?assertEqual(ok, R1),
    ?assertEqual(failed, R2),
    ?assertEqual(failed, R3),
    ?assertEqual(failed, R4),
    ?assertEqual(ok, R5),
    ?assertEqual(failed, R6),
    ok.

timeout_test() ->
    State0 = #state{},
    {reply, R1, State1} = handle_call({lock, 1}, from, State0),
    timer:sleep(250),
    {reply, R2, State2} = handle_call({lock, 2}, from, State1),
    timer:sleep(300),
    {reply, R3, _State3} = handle_call({lock, 2}, from, State2),
    ?assertEqual(ok, R1),
    ?assertEqual(failed, R2),
    ?assertEqual(ok, R3).

-endif.
