%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 25 May 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_utils).

-export([sysinfo/0, system/0]).


sysinfo() ->
    case system() of
        smartos ->
            SysInfo0 = jsxd:from_list(jsx:decode(list_to_binary(os:cmd("sysinfo")))),
            SysInfo = jsxd:delete([<<"Boot Parameters">>, <<"root_shadow">>], SysInfo0),
            {ok, SysInfo};
        omnios ->
            [];
        _ ->
            []
    end.

system() ->
    case os:cmd("uname -v") of
        "joyent" ++ _ ->
            smartos;
        "omnios" ++ _ ->
            omnios;
        _ ->
            undefined
    end.
