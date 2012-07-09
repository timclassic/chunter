
-module(chunter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    [Name|_] = re:split(os:cmd("uname -n"), "\n"),
    BaseKey = <<"chunter.", Name/binary>>,
    %application:set_env(statsderl, base_key, BaseKey),
    {ok, {{one_for_one, 5, 10}, [{vmstats_sup, 
				  {vmstats_sup, start_link, [<<BaseKey/binary, ".vmstats">>]}, permanent, 5000, supervisor, [vmstats_sup]},
				 ?CHILD(chunter_vm_sup, supervisor),
				 ?CHILD(chunter_server, worker),
				 ?CHILD(chunter_watchdog, worker)]}}.
