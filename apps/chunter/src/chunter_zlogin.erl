-module(chunter_zlogin).

-define(ZLOGIN_NODE, 'fifo_zlogin@127.0.0.1').

-export([start/2, stop/1, subscribe/1, send/2]).

start(UUID, Type) ->
    rpc:call(?ZLOGIN_NODE, zlogin_fsm, start, [UUID, Type]).

stop(UUID) ->
    gen_fsm:send_event({global, {zlogin, UUID}}, stop).

send(UUID, Data) ->
    gen_fsm:send_event({global, {zlogin, UUID}}, {send, Data}).

subscribe(UUID) ->
    gen_fsm:send_all_state_event({global, {zlogin, UUID}}, {subscribe, self()}).
