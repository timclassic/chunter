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
-export([start_link/0]).
-ignore_xref([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {port, doors=[], refs = []}).

-define(HEATRBEAT_INTERVAL, 1000). % 2 seconds - zonedoor terminates after 7 sec of no HB. Allow 2 misses

-define(DOOR, "_joyent_sshd_key_is_authorized").

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Cmd = code:priv_dir(chunter) ++ "/zonedoor",
    DoorPort = open_port({spawn_executable, Cmd},
                         [{args, []}, use_stdio, {line, 1024}, exit_status]),
    erlang:send_after(?HEATRBEAT_INTERVAL, self(), heartbeat),
    {ok, #state{port = DoorPort}}.

handle_call({verify_zonedoor, UUID}, _, State =
                #state{doors = Doors, refs = Refs, port = Port}) ->
    case lists:member(UUID, Doors) of
        false ->
            Ref = make_ref(),
            lager:info("[vm_auth] Request to add zone door: ~s~n", [UUID]),
            port_command(Port, [$a, UUID, $\s, ?DOOR, $\s, r2s(Ref), $\n]),
            State1 = State#state{doors = [UUID | Doors],
                                 refs = orddict:store(Ref, UUID, Refs)},
            {reply, ok, State1};
        _ ->
            {reply, ok, State}
    end;
handle_call({remove_zonedoor, UUID}, _, State =
                #state{doors = Doors, refs = Refs, port = Port}) ->
    case lists:member(UUID, State#state.doors) of
        true ->
            lager:info("[vm_auth] Request to delete zone door: ~s~n", [UUID]),
            port_command(Port, [$d, UUID, $\s, ?DOOR, $\n]),
            State1 = State#state{doors = lists:delete(UUID, Doors),
                                 refs = [{K, V} || {K, V} <- Refs, V /= UUID]},
            {reply, ok, State1};
        _ ->
            {reply, ok, State}
    end.

handle_info(heartbeat, State) ->
    port_command(State#state.port, "h\n"),
    erlang:send_after(?HEATRBEAT_INTERVAL, self(), heartbeat),
    {noreply, State};
handle_info({Port,{data,{eol,Data}}} , State = #state{port = Port}) ->
    lager:debug("[vm_auth] Received data: ~s~n", [Data]),
    auth_credintials(State, Data),
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

terminate(_, State) ->
    incinerate(State#state.port),
    ok.

%%%===================================================================
%%% Unused gen_server functions
%%%===================================================================

handle_cast(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

incinerate(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            port_close(Port),
            lager:warning("Killing ~p with -9", [OsPid]),
            os:cmd(io_lib:format("/usr/bin/kill -9 ~p", [OsPid]));
        undefined ->
            lager:warning("Process has no pid not incinerating.")
    end.

auth_credintials(#state{port = Port, refs = Refs}, Data) ->
    case re:split(Data, " ") of
        [RefStr,_,_,KeyID] ->
            case orddict:find(s2r(RefStr), Refs) of
                {ok, UUID} ->
                    auth_credintials(Port, [UUID,KeyID]),
                    ok;
                error ->
                    lager:warning("[zonedoor] unknown ref: ~s.", [RefStr]),
                    ok
            end;
        _ ->
            lager:warning("[zonedoor] recived unknown data: ~p.", [Data]),
            ok
    end;

auth_credintials(Port, [UUID,KeyID]) ->
    KeyBin = libsnarl:keystr_to_id(KeyID),
    case ls_user:key_find(KeyBin) of
        {ok, UserID} ->
            lager:warning("[zonedoor:~s] User ~s trying to connect with key ~s",
                          [UUID, UserID, KeyID]),
            case libsnarl:allowed(UserID, [<<"vms">>, UUID, <<"console">>]) orelse
                libsnarl:allowed(UserID, [<<"vms">>, UUID, <<"ssh">>]) of
                true ->
                    lager:warning("[zonedoor:~s] granted.", [UUID]),
                    port_command(Port, "r1\n");
                _ ->
                    lager:warning("[zonedoor:~s] denied.", [UUID]),
                    port_command(Port, "r0\n")
            end;
        _ ->
            lager:warning("[zonedoor:~s] denied.", [UUID]),
            port_command(Port, "r0\n")
    end.

r2s(Ref) ->
    base64:encode(term_to_binary(Ref)).

s2r(Str) ->
    binary_to_term(base64:decode(Str)).
