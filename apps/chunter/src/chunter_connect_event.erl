-module(chunter_connect_event).
-behaviour(gen_event).

-export([
         init/1,
         terminate/2,
         handle_event/2,
         code_change/3,
         handle_call/2,
         handle_info/2
        ]).

init(_) ->
    {ok, stateless}.

handle_event({connected, "sniffle"}, State) ->
    chunter_server:connect(),
    {ok, State};

handle_event({disconnected, "sniffle"}, State) ->
    chunter_server:disconnect(),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

terminate(_, _) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.
