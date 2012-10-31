%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_vm_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([creating/2,
	 stopped/2,
	 booting/2,
	 running/2,
	 shutting_down/2]).


-define(SERVER, ?MODULE).

-record(state, {uuid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([{create, UUID, Spec}]) ->
    {ok, creating, #state{uuid = UUID}};

init([{load, UUID}]) ->
    {ok, loading, #state{uuid = UUID}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, State) ->
    {next_state, state_name, State}.


creating(_Event, State) ->
    {next_state, creating, State}.

loading(_Event, State) ->
    {next_state, loading, State}.

stopped({force_state, UUID, NextState}, State = #state{uuid = UUID}) ->
    libsniffle:vm_attribute_set(UUID, <<"state">>, NextState),
    {next_state, NextState, State};

stopped({transition, NextState = booting}, State) ->
    libsniffle:vm_attribute_set(State.uuid, <<"state">>, NextState),
    {next_state, NextState, State};

stopped(start, State) ->
    chunter_vmadm:start(State.uuid),
    {next_state, stopped, State};

stopped(_, State) ->
    {next_state, stopped, State}.


booting({force_state, NextState}, State) ->
    libsniffle:vm_attribute_set(State.uuid, <<"state">>, NextState),
    {next_state, NextState, State};

booting({transition, NextState = shutting_down}, State) ->
    libsniffle:vm_attribute_set(State.uuid, <<"state">>, NextState),
    {next_state, NextState, State};

booting({transition, NextState = running}, State) ->
    libsniffle:vm_attribute_set(State.uuid, <<"state">>, NextState),
    {next_state, NextState, State};

booting(_, State) ->
    {next_state, booting, State}.


running({force_state, NextState}, State) ->
    libsniffle:vm_attribute_set(State.uuid, <<"state">>, NextState),
    {next_state, NextState, State};

running({transition, NextState = shutting_down}, State) ->
    libsniffle:vm_attribute_set(State.uuid, <<"state">>, NextState),
    {next_state, NextState, State};

running(_, State) ->
    {next_state, running, State}.


shutting_down({force_state, NextState}, State) ->
    libsniffle:vm_attribute_set(State.uuid, <<"state">>, NextState),
    {next_state, NextState, State};

shutting_down({transition, NextState = stopped}, State) ->
    libsniffle:vm_attribute_set(State.uuid, <<"state">>, NextState),
    {next_state, NextState, State};

shutting_down(_, State) ->
    {next_state, shutting_down, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
