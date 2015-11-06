-module(chunter_zlogin).


-export([start/2, subscribe/1, send/2]).

start(UUID, Type) ->
    rpc:call('zlogin_fifo@127.0.0.1', zlogin_fsm, start, [UUID, Type]).

send(UUID, Data) ->
    gen_fsm:send_event({global, {zlogin, UUID}}, {send, Data}).

subscribe(UUID) ->
    gen_fsm:send_all_state_event({global, {zlogin, UUID}}, {subscribe, self()}).
