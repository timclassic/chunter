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
         reboot/1
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

 
