
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
    ArkSrvs = case application:get_env(chunter, kstat_arc) of
                  {ok, false} ->
                      [];
                  _ ->
                      [?CHILD(chunter_kstat_arc, worker)]
              end,
    {ok, {{one_for_one, 5, 10},
          [
           ?CHILD(ezdoor_server, worker),
           ?CHILD(chunter_lock, worker),
           ?CHILD(chunter_dataset_srv, worker),
           ?CHILD(chunter_vm_sup, supervisor),
           ?CHILD(chunter_server, worker),
           ?CHILD(chunter_zpool_monitor, worker),
           ?CHILD(chunter_zonemon, worker)
          ] ++ ArkSrvs}}.
