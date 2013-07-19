-module(chunter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(chunter, port),
    {ok, _} = ranch:start_listener(chunter_server, 1,
                                   ranch_tcp, [{port, Port}], chunter_protocol, []),

    lager:info("chunter:load - start.", []),
    chunter_sup:start_link().

stop(_State) ->
    ok.
