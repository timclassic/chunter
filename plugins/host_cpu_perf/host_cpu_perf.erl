-module(host_cpu_perf).

-export([mpstat/0]).

mpstat() ->
    O = os:cmd("mpstat"),
    [Node |_] = re:split(os:cmd("uname -n"), "\n"),
    [_, Lines] = re:split(O, "\n"),
    Lines1 = lists:filter(fun(X) -> X =/= <<>> end, Lines),
    Lines2 = lists:map(fun(L) -> [_ | L1] = re:split(L, "\s+"), L1 end, Lines1),
    [{Node,
      [ [{<<"usr">>, Usr},
         {<<"sys">>, Sys},
         {<<"idl">>, Idl}]
        ||
          %% CPU minf mjf xcal intr ithr csw icsw migr smtx srw syscl usr  sys  wt idl
          [  _,  _,   _,  _,   _,   _,   _,  _,   _,   _,   _,  _,    Usr, Sys, _, Idl] <-
              Lines2
      ]}].
