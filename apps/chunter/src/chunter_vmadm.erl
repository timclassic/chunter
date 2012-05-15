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
	 create/1
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

create(Data) ->
    Cmd =  code:priv_dir(chunter) ++ "/vmadm_wrap.sh create",
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000}, stderr_to_stdout]),
    port_command(Port, jsx:to_json(Data)),
    port_command(Port, "\nEOF\n"),
    wait_for_tex(Port),
    port_close(Port).

    
wait_for_tex(Port) ->
    receive
	{Port,Text} ->
	    io:format("Data: ~p~n", [Text]),
            wait_for_tex(Port)
    after
	60000 ->
	    io:format("Timeout.~n")
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

 
