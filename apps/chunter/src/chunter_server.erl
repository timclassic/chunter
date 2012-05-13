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

%% API
-export([start_link/0, list/0, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {port}).

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

get(UUID) ->
    gen_server:call(?SERVER, {call, system, {machines, get, UUID}}).

list() ->
    gen_server:call(?SERVER, {call, system, {machines, list}}).
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
    {ok, #state{}, 1000}.


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
handle_call({call, Auth, {machines, list}}, _From, State) ->
    Reply = list_vms(),
    {reply, {ok, Reply}, State};

handle_call({call, Auth, {machines, get, UUID}}, _From, State) ->
    Reply = get_vm(UUID),
    {reply, {ok, Reply}, State};

% TODO
handle_call({call, Auth, {machines, info, UUID}}, _From, State) ->
    Reply = [], 
    {reply, {ok, Reply}, State};

handle_call({call, Auth, {packages, list}}, _From, State) ->
    Reply = [], 
    {reply, {ok,  Reply}, State};

handle_call({call, Auth, {datasets, list}}, _From, State) ->
    Reply = [], 
    {reply, {ok, Reply}, State};

handle_call({call, Auth, {keys, list}}, _From, State) ->
    Reply = [], 
    {reply, {ok, Reply}, State};


handle_call({call, Auth, Call}, _From, State) ->
    Reply = {error, {unsupported, Call}},
    {reply, Reply, State};

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
handle_cast({cast, Auth, {machines, start, UUID}}, State) ->
    spawn(chunter_vmadm, start, [UUID]),
    {noreply, State};

handle_cast({cast, Auth, {machines, start, UUID, Image}}, State) ->
    spawn(chunter_vmadm, start, [UUID, Image]),
    {noreply, State};


handle_cast({cast, Auth, {machines, stop, UUID}}, State) ->
    spawn(chunter_vmadm, stop, [UUID]),
    {noreply, State};

handle_cast({cast, Auth, {machines, reboot, UUID}}, State) ->
    spawn(chunter_vmadm, reboot, [UUID]),
    {noreply, State};


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
handle_info(timeout, State) ->
    libsniffle:register(system, chunter, self()),
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


get_vm(ZUUID) ->
    [VM] = [chunter_zoneparser:load([{name,Name},{state, VMState},{pathzonepath, Path},{uuid, UUID},{type, Type}]) || 
	       [ID,Name,VMState,Path,UUID,Type,_IP,_SomeNumber] <- 
		   [ re:split(Line, ":") 
		     || Line <- re:split(os:cmd("/usr/sbin/zoneadm -u" ++ binary_to_list(ZUUID) ++ " list -p"), "\n")],
	       ID =/= <<"0">>],
    VM.


list_vms() ->
    [chunter_zoneparser:load([{name,Name},{state, VMState},{pathzonepath, Path},{uuid, UUID},{type, Type}]) || 
	[ID,Name,VMState,Path,UUID,Type,_IP,_SomeNumber] <- 
	    [ re:split(Line, ":") 
	      || Line <- re:split(os:cmd("/usr/sbin/zoneadm list -ip"), "\n")],
	ID =/= <<"0">>].
    
