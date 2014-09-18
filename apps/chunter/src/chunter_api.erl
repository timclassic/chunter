-module(chunter_api).

-export([call/2]).

call(UUID, <<"snapshot create ", Comment/binary>>) ->
    case ls_vm:snapshot(UUID, Comment) of
        {ok, SUUID} ->
            Bin = jsx:encode([{uuid, SUUID}]),
            {ok, <<$1, Bin/binary>>};
        _ ->
            {ok, <<$0, "failed!">>}
    end;
call(_, Cmd) ->
    {ok, <<0, "unsupported: ", Cmd/binary>>}.
