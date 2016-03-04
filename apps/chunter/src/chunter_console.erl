%% @doc Interface for howl-admin commands.
-module(chunter_console).
-export([connections/1]).
-ignore_xref([connections/1]).

-export([update_mem/1]).
-ignore_xref([update_mem/1]).


connections(["snarl"]) ->
    io:format("Snarl endpoints.~n"),
    print_endpoints(libsnarl:servers());

connections(["sniffle"]) ->
    io:format("Sniffle endpoints.~n"),
    print_endpoints(libsniffle:servers());

connections(["howl"]) ->
    io:format("Howl endpoints.~n"),
    print_endpoints(libsniffle:servers());

connections([]) ->
    connections(["snarl"]),
    connections(["sniffle"]),
    connections(["howl"]).

update_mem([]) ->
    chunter_server:update_mem().

print_endpoints(Es) ->
    io:format("Hostname            "
              "                    "
              " Node               "
              " Errors    ~n"),
    io:format("--------------------"
              "----------"
              " --------------------"
              " ---------------~n", []),
    [print_endpoint(E) || E <- Es].

print_endpoint({{Hostname, [{port, Port}, {ip, IP}]}, _, Fails}) ->
    HostPort = <<IP/binary, ":", Port/binary>>,
    io:format("~30s ~-24s ~9b~n", [Hostname, HostPort, Fails]).
