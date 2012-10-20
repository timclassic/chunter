%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  2 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter).

%% API
-export([start/0]).

-ignore_xref([start/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start() ->
    application:start(sasl),
    application:start(lager),
    application:start(nicedecimal),
    application:start(jsx),
    application:start(crypto),
    application:start(inets),
    application:start(erlsom),
    application:start(zmq_mdns_client),
    application:start(libsniffle),
    application:start(libsnarl),
    application:start(gproc),
    application:start(ranch),
    application:start(chunter).

%%%===================================================================
%%% Internal functions
%%%===================================================================
