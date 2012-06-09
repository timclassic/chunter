%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_vmadm).

-include_lib("alog_pt.hrl").

%% API
-export([start/1,
         start/2,
         stop/1,
	 info/1,
         reboot/1,
	 delete/1,
	 create/4
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start(UUID) ->
    ?INFO({start, UUID}, [], [vmadm, chunter]),
    Cmd = <<"/usr/sbin/vmadm start ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

delete(UUID) ->
    ?INFO({delete, UUID}, [], [vmadm, chunter]),
    Cmd = <<"/usr/sbin/vmadm delete ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

info(UUID) ->
    ?INFO({info, UUID}, [], [vmadm, chunter]),
    Cmd = <<"/usr/sbin/vmadm info ", UUID/binary>>,
    chunter_server:niceify_json(jsx:to_term(list_to_binary(os:cmd(binary_to_list(Cmd))))).

start(UUID, Image) ->
    ?INFO({start, UUID, Image}, [], [vmadm, chunter]),
    Cmd = <<"/usr/sbin/vmadm start ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

stop(UUID) ->
    ?INFO({stop, UUID}, [], [vmadm, chunter]),
    Cmd = <<"/usr/sbin/vmadm stop ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

reboot(UUID) ->
    ?INFO({reboot, UUID}, [], [vmadm, chunter]),
    Cmd = <<"/usr/sbin/vmadm reboot", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

create(Data, Caller, Owner, Rights) ->
    ?INFO({create, Data, Caller, Owner, Rights}, [], [vmadm, chunter]),
    Cmd =  code:priv_dir(chunter) ++ "/vmadm_wrap.sh create",
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000}, stderr_to_stdout]),
    port_command(Port, jsx:to_json(Data)),
    port_command(Port, "\nEOF\n"),
    Res = case wait_for_tex(Port) of
	      {ok, UUID} ->
		  {ok, Owners} = libsnarl:group_add(system, <<"vm_", UUID/binary, "_owner">>),
		  [libsnarl:group_grant(system, Owners, Perm) ||
		      Perm <- [[vm, UUID, '...'] | Rights]],
		  libsnarl:user_add_to_group(system, Owner, Owners),
		  {ok, chunter_server:get_vm(UUID)};
	      E ->
		  ?ERROR({create, error, E}, [], [vmadm, chunter]),
		  E
	  end,
    gen_server:reply(Caller, Res),
    Res.

wait_for_tex(Port) ->
    receive
	{Port, {data,{eol,<<"Successfully created ", UUID/binary>>}}} ->
	    ?INFO({vmadm, success, UUID}, [], [vmadm, chunter]),
            {ok, UUID};
	{Port, {data, {eol, Text}}} ->
	    ?WARNING({vmadm, unknown, Text}, [], [vmadm, chunter]),
            {error, Text};
        {Port, E} ->
	    ?ERROR({vmadm, error, E}, [], [vmadm, chunter]),
            {error, unknown}
    after
	60000 ->
	    ?ERROR({vmadm, timeout}, [], [vmadm, chunter]),
            {error, timeout}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
