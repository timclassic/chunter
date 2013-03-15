-module(host_cpu_perf).

-export([mpstat/0]).

mpstat() ->
    [Node |_] = re:split(os:cmd("uname -n"), "\n"),
    Data = re:split(os:cmd("/usr/bin/kstat -p cpu | grep sys:cpu_nsec_ | awk '{print $2}'"), "\n"),
    [{Node,
      [{<<"event">>, <<"mpstat">>},
       {<<"data">>, build_obj(Data)}]}].


build_obj([Idel, Intr, Kernel, User | R]) ->
    [[{<<"usr">>, list_to_integer(binary_to_list(User))},
      {<<"sys">>, list_to_integer(binary_to_list(Kernel))},
      {<<"int">>, list_to_integer(binary_to_list(Intr))},
      {<<"idl">>, list_to_integer(binary_to_list(Idel))}]] ++
        build_obj(R);

build_obj(_) ->
    [].
