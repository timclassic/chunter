-module(chunter_docker).

-export([import/1]).

-define(IMGADM, "/usr/sbin/imgadm").

import(Image) ->
    case chunter_cmd:run(?IMGADM, ["import", q, Image]) of
        {ok, <<"Importing ", UUID:36/binary, _/binary>>} ->
            {ok, UUID};
        {ok, <<"Image ", UUID:36/binary, _/binary>>} ->
            {ok, UUID};
        Error ->
            lager:error("[docker] Could not import image ~s: ~p", [Image, Error]),
            Error
    end.
