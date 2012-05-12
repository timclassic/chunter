-module(chunter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, load/0]).


load() ->
    application:start(sasl),
    application:start(alog),
    application:start(redgrid),
    application:start(gproc),
    application:start(nicedecimal),
    application:start(jsx),
    application:start(inets),
    application:start(erlsom),
    application:start(libsniffle),
    application:start(chunter).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    chunter_sup:start_link().

stop(_State) ->
    ok.
