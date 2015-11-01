-module(chunter_docker).

-export([get_uuid/1]).

-define(IMGADM, "/usr/sbin/imgadm").

get_uuid(Image) ->
    case chunter_cmd:run_json(?IMGADM, ["show", Image]) of
        {ok, JSON} ->
            jsxd:get([<<"uuid">>], JSON);
        Error ->
            lager:error("[docker] Could not find image ~s: ~p", [Error]),
            not_found
    end.
