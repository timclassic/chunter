-module(ezdoor_behaviour).

-callback door_event(Pid::pid(), Ref::reference(), Cmd::binary()) ->
    {ok, Reply::binary()|iolist()}.
