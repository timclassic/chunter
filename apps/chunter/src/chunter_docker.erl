-module(chunter_docker).

-export([import/1, get_uuid/1]).

-define(IMGADM, "/usr/sbin/imgadm").

get_uuid(Image) ->
    case chunter_cmd:run_json(?IMGADM, ["show", Image]) of
        {ok, JSON} ->
            jsxd:get([<<"uuid">>], JSON);
        Error ->
            lager:error("[docker] Could not find image ~s: ~p", [Image, Error]),
            not_found
    end.

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
