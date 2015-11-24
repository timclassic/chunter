-module(ezdoor_behaviour).

-ignore_xref([behaviour_info/1]).
-export([event/4]).

-callback door_event(Pid::pid(), Ref::reference(), Cmd::binary()|down) ->
    {ok, Reply::binary()|iolist()} | ok.

event(Mod, Pid, Ref, Cmd) ->
    Mod:door_event(Pid, Ref, Cmd).
