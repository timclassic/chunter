-module(chunter_api).

-export([call/2]).

call(UUID, <<"snapshot create ", Comment/binary>>) ->
    case ls_vm:snapshot(UUID, Comment) of
        {ok, SUUID} ->
            {ok, [{uuid, SUUID}]};
        _ ->
            {error, <<"failed!">>}
    end;
call(_, Cmd) ->
    {error, <<"unsupported: ", Cmd/binary>>}.
