-module(chunter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, load/0]).

check_grid() ->
    timer:sleep(100),
    Nodes = redgrid:nodes(),
    case length(Nodes) of
	X when X =< 1 ->
	    lager:warning("chunter:load - Not enough nodes: ~p",
			  [Nodes]),
	    check_grid();
	_ ->
	    lager:warning("chunter:load - redgrid connection established.", []),
	    application:stop(gproc),
	    application:start(gproc),
	    ok
    end.


load() ->
    application:start(sasl),
    application:start(lager),
    application:start(alog),
    application:start(redgrid),
    application:start(gproc),
    application:start(nicedecimal),
    application:start(jsx),
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
    check_grid(),
    chunter_sup:start_link().

stop(_State) ->
    ok.
