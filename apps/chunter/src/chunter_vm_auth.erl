%%%-------------------------------------------------------------------
%%% @author Kevin Meziere <kevin@meziere.com>
%%% @copyright (C) 2014, Kevin Meziere
%%% @doc
%%%
%%% @end
%%% Created : 01 Aug 2014 by Kevin Meziere <kevin@meziere.com>
%%%-------------------------------------------------------------------
-module(chunter_vm_auth).

-behaviour(gen_server).
-behaviour(ezdoor_behaviour).

-export([start_link/0, door_event/3]).
-ignore_xref([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {refs = []}).

-define(DOOR, "_joyent_sshd_key_is_authorized").

door_event(Pid, Ref, Data) ->
    gen_server:call(Pid, {door, Ref, Data}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #state{}}.

handle_call({verify_zonedoor, UUID}, _, State = #state{refs = Refs}) ->
    case ezdoor_server:add(?MODULE, UUID, ?DOOR) of
        {ok, Ref} ->
            {reply, ok, State#state{refs = orddict:store(Ref, UUID, Refs)}};
        {error, doublicate} ->
            {reply, ok, State}
    end;

handle_call({remove_zonedoor, UUID}, _, State =#state{refs = Refs}) ->
    [Ref] = [Ref || {Ref, AUUID} <- Refs, AUUID == UUID],
    ezdoor_server:remove(Ref),
    {reply, ok, State#state{refs = orddict:erase(Ref, Refs)}};

handle_call({door, Ref, Data}, _, State = #state{refs = Refs}) ->
    case orddict:find(Ref, Refs) of
        {ok, UUID} ->
            R = case auth_credintials(UUID, Data) of
                    true ->
                        <<"1">>;
                    false ->
                        <<"0">>
                end,
            {reply, {ok, R}, State};
        error ->
            {reply, {ok, <<"0">>}, State}
    end.

handle_info(_Msg , State) ->
    {noreply, State}.

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

terminate(_, _State) ->
    ok.

%%%===================================================================
%%% Unused gen_server functions
%%%===================================================================

handle_cast(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

auth_credintials(UUID, Data) ->
    case re:split(Data, " ") of
        [User, _UserID, KeyID] ->
            chk_key(UUID, User, KeyID);
        _ ->
            lager:warning("[zonedoor] recived unknown data: ~p.", [Data]),
            false
    end.

chk_key(UUID, User, KeyID) ->
    KeyBin = libsnarl:keystr_to_id(KeyID),
    case ls_user:key_find(KeyBin) of
        {ok, UserID} ->
            Res = libsnarl:allowed(UserID, [<<"vms">>, UUID, <<"console">>])
                orelse libsnarl:allowed(UserID, [<<"vms">>, UUID, <<"ssh">>]),
            lager:warning("[zonedoor:~s] User ~s trying to connect with key ~s -> ~s",
                          [UUID, User, KeyID, Res]),
            Res;
        _ ->
            lager:warning("[zonedoor:~s] denied.", [UUID]),
            false
    end.
