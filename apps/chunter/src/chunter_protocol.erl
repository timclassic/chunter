-module(chunter_protocol).
-export([start_link/4, init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(ListenerPid),
    State = stateless,
    loop(Socket, Transport, State).

loop(Socket, Transport, HandlerState) ->
    case Transport:recv(Socket, 0, 5000) of
	{ok, BinData} ->
	    case binary_to_term(BinData) of
		ping ->
		    Transport:send(Socket, <<"pong">>),
		    ok = Transport:close(Socket);
		Data ->
		    case handle_message(Data, HandlerState) of
			{reply, Reply, HandlerState1} ->
			    Transport:send(Socket, term_to_binary({reply, Reply})),
			    loop(Socket, Transport, HandlerState1);
			{noreply, HandlerState1} ->
			    Transport:send(Socket, term_to_binary(noreply)),
			    loop(Socket, Transport, HandlerState1);
			{stop, Reply, _} ->
			    Transport:send(Socket, term_to_binary({reply, Reply})),
			    ok = Transport:close(Socket);
			{stop, _} ->
			    ok = Transport:close(Socket)
		    end
	    end;
	_ ->
	    ok = Transport:close(Socket)
    end.

handle_message({machines, start, UUID}, State) ->
    io:format("start~n"),
    chunter_server:start(UUID),
    {stop, State};

handle_message({machines, start, UUID, Image}, State) ->
    io:format("start1~n"),
    chunter_server:start(UUID, Image),
    {stop, State};

handle_message({machines, stop, UUID}, State) ->
    io:format("stop~n"),
    chunter_server:stop(UUID),
    {stop, State};

handle_message({machines, reboot, UUID}, State) ->
    io:format("reboot~n"),
    chunter_server:reboot(UUID),
    {stop, State};

handle_message({machines, create, UUID, Name, PackageUUID, DatasetUUID, Metadata, Tags}, State) ->
    io:format("create~n"),
    chunter_server:create(UUID, Name, PackageUUID, DatasetUUID, Metadata, Tags),
    {stop, State};

handle_message({machines, delete, UUID}, State) ->
    io:format("delete~n"),
    chunter_server:delete(UUID),
    {stop, State};

handle_message(Oops, State) ->
    io:format("oops: ~p~n", [Oops]),
    {stop, State}.

