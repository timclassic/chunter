-module(host_cpu_perf).

-export([mpstat/0]).

mpstat() ->
    [Node |_] = re:split(os:cmd("uname -n"), "\n"),
    Data = re:split(os:cmd("/usr/bin/kstat -p cpu | grep sys:cpu_nsec_ | awk '{print $2}'"), "\n"),
    [{Node,
      [{<<"event">>, <<"mpstat">>},
       {<<"data">>, build_obj(Data)}]}].


build_obj([Idel, Intr, Kernel, User | R]) ->
    [[{<<"usr">>, to_number(User)},
      {<<"sys">>, to_number(Kernel)},
      {<<"int">>, to_number(Intr)},
      {<<"idl">>, to_number(Idel)}]] ++
        build_obj(R);

build_obj(_) ->
    [].

to_number(N) ->
    case re:run(N, "\\.") of
        nomatch ->
            list_to_integer(binary_to_list(N));
        _ ->
            list_to_float(binary_to_list(N))
    end.
