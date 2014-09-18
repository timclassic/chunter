-module(chunter_api).

-export([call/2]).

call(UUID, <<"snapshot ", Comment/binary>>) ->
    case ls_vm:snapshot(UUID, Comment) of
        {ok, UUID} ->
            {ok, UUID};
        _ ->
            {ok, <<"failed!">>}
    end;
call(_, Cmd) ->
    {ok, <<"unsupported: ", Cmd/binary>>}.
