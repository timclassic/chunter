-module(chunter_docker).

-export([get_uuid/1]).

get_uuid(Image) ->
    case chunter_cmd:run_json("imgadm", ["show", Image]) of
        {ok, JSON} ->
            jsxd:get([<<"uuid">>], JSON);
        Error ->
            lager:error("[docker] Could not find image ~s: ~p", [Error]),
            not_found
    end.
