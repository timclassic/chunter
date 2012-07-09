
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
    application:set_env(statsderl, base_key, <<"chunter."/string, Name>>),
    {ok, {{one_for_one, 5, 10}, [?CHILD(vmstats_sup, supervisor),
				 ?CHILD(chunter_vm_sup, supervisor),
				 ?CHILD(chunter_server, worker),
				 ?CHILD(chunter_watchdog, worker)]}}.
