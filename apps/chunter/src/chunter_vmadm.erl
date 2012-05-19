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
         reboot/1,
	 create/2
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


%TODO
start(UUID, Image) ->
    Cmd = <<"/usr/sbin/vmadm start ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).


stop(UUID) ->
    Cmd = <<"/usr/sbin/vmadm stop ", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

reboot(UUID) ->
    Cmd = <<"/usr/sbin/vmadm reboot", UUID/binary>>,
    os:cmd(binary_to_list(Cmd)).

create(Data, Caller) ->
    Cmd =  code:priv_dir(chunter) ++ "/vmadm_wrap.sh create",
    io:format("1~n"),
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000}, stderr_to_stdout]),
    io:format("2~n"),
    port_command(Port, jsx:to_json(Data)),
    io:format("3~n"),
    port_command(Port, "\nEOF\n"),
    io:format("4~n"),
    Res = case wait_for_tex(Port) of
	      {ok, UUID} ->
		  io:format("5a: ~p~n", [UUID]),
		  {ok, chunter_server:get_vm(UUID)};
	      E ->
		  io:format("5b: ~p~n", [E]),
		  E
	  end,
    gen_server:reply(Caller, Res),
    io:format("6: ~p~n", [Res]),
    port_close(Port),
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

 
