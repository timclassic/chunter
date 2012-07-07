-module(chunter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, load/0]).

load() ->
    application:start(sasl),
    application:start(lager),
    application:start(alog),
    application:start(nicedecimal),
    application:start(jsx),
    application:start(crypto),
    application:start(nodefnder),
    application:start(backyard),
    application:start(inets),
    application:start(erlsom),
    application:start(libsniffle),
    application:start(libsnarl),
    application:start(chunter).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:warning("chunter:load - start.", []),
    chunter_sup:start_link().

stop(_State) ->
    ok.
