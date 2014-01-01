-module(chunter_snap).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         describe_restore/1,
         download/3,
         download_to_port/3,
         upload/4,
         get/1,
         get_all/2,
         restore_path/3,
         mk_s3_conf/1
        ]).

upload(<<_:1/binary, P/binary>>, VM, SnapID, Options) ->
    Disk = case P of
               %% 42 is the lenght of zone/<uuid>
               <<_:42/binary, "-", Dx/binary>> ->
                   <<"-", Dx/binary>>;
               _ ->
                   <<>>
           end,
    Target = <<SnapID/binary, Disk/binary>>,
    Chunk = case application:get_env(chunter, upload_chunk) of
                undefined ->
                    1048576;
                {ok, S} ->
                    S
            end,
    lager:debug("Starting upload wiht chunk size: ~p.", [Chunk]),
    AKey = proplists:get_value(access_key, Options),
    SKey = proplists:get_value(secret_key, Options),
    S3Host = proplists:get_value(s3_host, Options),
    S3Port = proplists:get_value(s3_port, Options),
    Bucket = proplists:get_value(s3_bucket, Options),
    {ok, Upload} = fifo_s3_upload:new(AKey, SKey, S3Host, S3Port, Bucket, Target),
    Cmd = code:priv_dir(chunter) ++ "/zfs_export.gzip.sh",
    Prt = case proplists:get_value(parent, Options) of
              undefined ->
                  lager:debug("Running ZFS command: ~p ~s ~s",
                              [Cmd, P, SnapID]),
                  open_port({spawn_executable, Cmd},
                            [{args, [P, SnapID]}, use_stdio, binary,
                             stderr_to_stdout, exit_status, stream]);
              Inc ->
                  libsniffle:vm_set(
                    VM, [<<"backups">>, SnapID, <<"parent">>], Inc),
                  lager:debug("Running ZFS command: ~p ~s ~s ~s",
                              [Cmd, P, SnapID, Inc]),
                  open_port({spawn_executable, Cmd},
                            [{args, [P, SnapID, Inc]}, use_stdio, binary,
                             stderr_to_stdout, exit_status, stream])
          end,
    libsniffle:vm_set(VM, [<<"backups">>, SnapID, <<"state">>], <<"uploading">>),
    {ok, VMObj} = libsniffle:vm_get(VM),
    Size = jsxd:get([<<"backups">>, SnapID, <<"size">>], 0, VMObj),
    Fs = jsxd:get([<<"backups">>, SnapID, <<"files">>], [], VMObj),
    libsniffle:vm_set(VM, [<<"backups">>, SnapID, <<"files">>], [Target | Fs]),
    upload_to_cloud(VM, SnapID, Prt, Upload, <<>>, Chunk, Size, Options).


upload_to_cloud(UUID, SnapID, Port, Upload, AccIn, Chunk, Size, Options) ->
    case AccIn of
        <<MB:Chunk/binary, Acc/binary>> ->
            case fifo_s3_upload:part(Upload, binary:copy(MB)) of
                ok ->
                    lager:debug("Uploading part: ~p.", [Size]),
                    libsniffle:vm_set(
                      UUID, [<<"backups">>, SnapID, <<"size">>],
                      Size),
                    upload_to_cloud(UUID, SnapID, Port, Upload, Acc, Chunk, Size,
                                    Options);
                {error, E} ->
                    fifo_s3_upload:abort(Upload),
                    lager:error("Upload error: ~p", [E]),
                    libsniffle:vm_set(
                      UUID, [<<"snapshots">>, SnapID, <<"state">>],
                      <<"upload failed">>),
                    {error, 3, E}
            end;
        Acc ->
            receive
                {Port, {data, Data}} ->
                    Size1 = Size + byte_size(Data),
                    upload_to_cloud(UUID, SnapID, Port, Upload,
                                    <<Acc/binary, Data/binary>>, Chunk, Size1, Options);
                {Port, {exit_status, 0}} ->
                    R = case Acc of
                            <<>> ->
                                fifo_s3_upload:done(Upload),
                                ok;
                            _ ->
                                case fifo_s3_upload:part(Upload, binary:copy(Acc)) of
                                    ok ->
                                        fifo_s3_upload:done(Upload),
                                        lager:debug("Upload done."),
                                        ok;
                                    {error, Err} ->
                                        lager:error("Upload error: ~p", [Err]),
                                        {error, Err}
                                end
                        end,
                    case R of
                        ok ->
                            lager:debug("Upload complete: ~p.", [Size]),
                            libsniffle:vm_set(
                              UUID, [<<"backups">>, SnapID, <<"size">>], Size),
                            M = io_lib:format("Uploaded ~s with a total size of done: "
                                              "~p", [UUID, Size]),

                            {ok, list_to_binary(M)};
                        {error, E} ->
                            fifo_s3_upload:abort(Upload),
                            libsniffle:vm_set(
                              UUID, [<<"backups">>, SnapID, <<"state">>],
                              <<"upload failed">>),
                            {error, 2, E}
                    end;
                {Port, {exit_status, S}} ->
                    fifo_s3_upload:abort(Upload),
                    lager:error("Upload error: ~p", [S]),
                    libsniffle:vm_set(
                      UUID, [<<"backups">>, SnapID, <<"state">>],
                      <<"upload failed">>),
                    {error, S, <<"upload failed">>}
            end
    end.

download(<<_:1/binary, P/binary>>, SnapID, Options) ->
    Disk = case P of
               %% 42 is the lenght of zone/<uuid>
               <<_:42/binary, "-", Dx/binary>> ->
                   <<"-", Dx/binary>>;
               _ ->
                   <<>>
           end,
    AKey = proplists:get_value(access_key, Options),
    SKey = proplists:get_value(secret_key, Options),
    S3Host = proplists:get_value(s3_host, Options),
    S3Port = proplists:get_value(s3_port, Options),
    Bucket = proplists:get_value(s3_bucket, Options),
    Target = <<SnapID/binary, Disk/binary>>,
    Chunk = case application:get_env(chunter, download_chunk) of
                undefined ->
                    1048576;
                {ok, S} ->
                    S
            end,
    lager:debug("Starting download wiht chunk size: ~p.", [Chunk]),
    {ok, Download} = fifo_s3_download:new(AKey, SKey, S3Host, S3Port, Bucket,
                                          Target, [{chunk_size, Chunk}]),
    Cmd = code:priv_dir(chunter) ++ "/zfs_import.gzip.sh",
    lager:debug("Running ZFS command: ~p ~s ~s", [Cmd, P, SnapID]),
    Prt = open_port({spawn_executable, Cmd},
                    [{args, [P, SnapID]}, use_stdio, binary,
                     stderr_to_stdout, exit_status, stream]),
    download_to_port(Prt, Download, 0).

download_to_port(Prt, Download, I) ->
    case fifo_s3_download:get(Download) of
        {ok, done} ->
            lager:debug("Download complete: ~p.", [I]),
            port_close(Prt),
            {ok, done};
        {ok, Data} ->
            lager:debug("Download part: ~p.", [I]),
            port_command(Prt, Data),
            download_to_port(Prt, Download, I+1);
        {error, E} ->
            port_close(Prt),
            lager:error("Import error: ~p", [I, E]),
            {error, 0, E}
    end.

describe_restore([{local, U} | R]) ->
    lager:debug("[restore] Using local snapshot ~s", [U]),
    describe_restore(R);
describe_restore([{full, U} | R]) ->
    lager:debug("[restore] Using full backup ~s", [U]),
    describe_restore(R);
describe_restore([{incr, U} | R]) ->
    lager:debug("[restore] incremental update to ~s", [U]),
    describe_restore(R);
describe_restore([]) ->
    ok.

get(UUID) ->
    Cmd = "/usr/sbin/zfs list -rpH -t snapshot zones/" ++ binary_to_list(UUID),
    Res = os:cmd(Cmd),
    Ls = [L || L <- re:split(Res, "\n"), L =/= <<>>],
    Ls1 = [re:run(L, ".*@(.*?)\t.*", [{capture, [1], binary}]) || L <- Ls],
    [M || {match,[M]} <- Ls1].

%% @doc Sum up the sizes of the original snapshot and the size of disks for
%% KVM machines.
get_all(VM, Spec) ->
    Disks = jsxd:get(<<"disks">>, [], Spec),
    Disks1 = [jsxd:get(<<"path">>, <<"">>, D) || D <- Disks],
    Disks2 = [D || <<_:15/binary, D/binary>> <- Disks1],
    Lines = lists:foldl(
              fun(Disk, LAcc) ->
                      LAcc ++ lines(Disk)
              end,
              lines("zones/" ++ binary_to_list(VM)),
              Disks2),
    Snaps = [{lists:last(re:split(Name, "@")),
              list_to_integer(binary_to_list(Size))}
             || [Name, Size, _, _, _] <- Lines],
    lists:foldl(fun({ID, S}, [{ID, SA} | A]) ->
                        [{ID, SA + S} | A];
                   (S, A) ->
                        [S | A]
                end, [], lists:sort(Snaps)).


lines(Disk) when is_binary(Disk) ->
    lines(binary_to_list(Disk));

lines(Disk) ->
    Cmd = "/usr/sbin/zfs list -r -t snapshot -pH " ++ Disk,
    lager:debug("Getting snapshots: ~s", [Cmd]),
    Data = os:cmd(Cmd),
    [re:split(L, "\t") || L <-re:split(Data, "\n"),
                          L =/= <<>>].


restore_path(Target, Remote, Local) ->
    restore_path(Target, Remote, Local, []).

restore_path(Target, Remote, Local, Path) ->
    case lists:member(Target, Local) of
        true ->
            {ok, [{local, Target} | Path]};
        false ->
            case jsxd:get(Target, Remote) of
                {ok, Snap} ->
                    case jsxd:get(<<"parent">>, Snap) of
                        {ok, Parent} ->
                            restore_path(Parent, Remote, Local,
                                         [{incr, Target} | Path]);
                        _ ->
                            {ok, [{full, Target} | Path]}
                    end;
                _ ->
                    {error, nopath}
            end
    end.


mk_s3_conf(Options) ->
    AKey = proplists:get_value(access_key, Options),
    SKey = proplists:get_value(secret_key, Options),
    S3Host = proplists:get_value(s3_host, Options),
    S3Port = proplists:get_value(s3_port, Options),
    fifo_s3:make_config(AKey, SKey, S3Host, S3Port).

-ifdef(TEST).
restore_path_test() ->
    Local = [<<"b">>, <<"c">>],
    Remote = [
              {<<"a">>, [{<<"parent">>, <<"c">>}]},
              {<<"d">>, [{<<"parent">>, <<"e">>}]},
              {<<"e">>, [{<<"parent">>, <<"f">>}]},
              {<<"f">>, []},
              {<<"g">>, [{<<"parent">>, <<"h">>}]}
             ],
    {ok, ResA} = restore_path(<<"a">>, Remote, Local),
    {ok, ResB} = restore_path(<<"b">>, Remote, Local),
    {ok, ResD} = restore_path(<<"d">>, Remote, Local),
    ResG = restore_path(<<"g">>, Remote, Local),

    ?assertEqual([{local, <<"b">>}], ResB),
    ?assertEqual([{local, <<"c">>}, {incr, <<"a">>}], ResA),
    ?assertEqual([{full, <<"f">>}, {incr, <<"e">>}, {incr, <<"d">>}], ResD),
    ?assertEqual({error, nopath}, ResG),
    ok.

-endif.
