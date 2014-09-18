-module(ezdoor_behaviour).

-callback door_event(Pid::pid(), Ref::reference(), Cmd::binary()|down) ->
    {ok, Reply::binary()|iolist()} | ok.
