%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_vmadm).

%% API
-export([start/1,
         start/2,
         stop/1,
	 info/1,
         reboot/1,
	 delete/1,
	 create/3
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
    Cmd = <<"/usr/sbin/vmadm start ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

delete(UUID) ->
    Cmd = <<"/usr/sbin/vmadm delete ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

info(UUID) ->
    Cmd = <<"/usr/sbin/vmadm info ", UUID/binary>>,
    chunter_server:niceify_json(jsx:to_term(list_to_binary(os:cmd(binary_to_list(Cmd))))).

start(UUID, Image) ->
    Cmd = <<"/usr/sbin/vmadm start ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

stop(UUID) ->
    Cmd = <<"/usr/sbin/vmadm stop ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

reboot(UUID) ->
    Cmd = <<"/usr/sbin/vmadm reboot", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

create(Data, Caller, Owner) ->
    Cmd =  code:priv_dir(chunter) ++ "/vmadm_wrap.sh create",
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000}, stderr_to_stdout]),
    port_command(Port, jsx:to_json(Data)),
    port_command(Port, "\nEOF\n"),
    Res = case wait_for_tex(Port) of
	      {ok, UUID} ->
		  libsnarl:user_grant(system, Owner, [vm, UUID, '...']),
		  {ok, chunter_server:get_vm(UUID)};
	      E ->
		  E
	  end,
    gen_server:reply(Caller, Res),
    Res.

    
wait_for_tex(Port) ->
    receive
	{Port, {data,{eol,<<"Successfully created ", UUID/binary>>}}} ->
            {ok, UUID};
	{Port, {data, {eol, Text}}} ->
            {error, Text};
        {Port, _E} ->
            {error, unknown}
    after
	60000 ->
            {error, timeout}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

 
