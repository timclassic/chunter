-module(chunter_api).

-export([call/2]).

-define(MAX_MDATA_SIZE, 1024*1024*32).

call(UUID, [{<<"action">>, <<"snapshot-create">>},
            {<<"comment">>, Comment}]) ->
    case ls_vm:snapshot(UUID, Comment) of
        {ok, SUUID} ->
            {ok, [{uuid, SUUID}]};
        _ ->
            {error, "failed!"}
    end;

call(UUID, [{<<"action">>, <<"snapshot-list">>}]) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            {ok, ft_vm:snapshots(V)};
        _ ->
            {error, "failed!"}
    end;

call(UUID, [{<<"action">>, <<"snapshot-get">>},
            {<<"uuid">>, SnapID}]) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            Snaps = ft_vm:snapshots(V),
            case jsxd:get(SnapID, Snaps) of
                {ok, S} ->
                    {ok, S};
                _ ->
                    {error, "not found"}
            end;
        _ ->
            {error, "failed!"}
    end;

call(UUID, [{<<"action">>, <<"metadata-get">>}]) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            {ok, ft_vm:metadata(V)};
        _ ->
            {error, "failed!"}
    end;

call(UUID, [{<<"action">>, <<"metadata-set">>},
            {<<"data">>, D}]) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            Size = byte_size(term_to_binary(ft_vm:metadata(V))) +
                byte_size(term_to_binary(D)),
            if
                Size > ?MAX_MDATA_SIZE ->
                    {error, "out of space"};
                true ->
                    ls_vm:set_metadata(UUID, D),
                    {ok, V1} = ls_vm:get(UUID),
                    {ok, ft_vm:metadata(V1)}
            end;
        _ ->
            {error, "failed!"}
    end;


call(UUID, [{<<"action">>, <<"backup-list">>}]) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            {ok, ft_vm:backups(V)};
        _ ->
            {error, "failed!"}
    end;

call(UUID, [{<<"action">>, <<"backup-create">>},
            {<<"comment">>, Comment},
            {<<"delete">>, Delete}]) ->
    Opts = case Delete of
               true ->
                   [delete, xml];
               _ ->
                   [xml]
           end,
    case ls_vm:full_backup(UUID, Comment, Opts) of
        {ok, BackupID} ->
            {ok, BackupID};
        _ ->
            {error, "failed!"}
    end;
call(UUID, [{<<"action">>, <<"backup-create">>},
            {<<"comment">>, Comment},
            {<<"delete">>, Delete},
            {<<"parent">>, Parent}]) ->
    Opts = case Delete of
               true ->
                   [delete, xml];
               <<"parent">> ->
                   [{delete, parent}, xml];
               <<"both">> ->
                   [{delete, parent}, delete, xml];
               _ ->
                   [xml]
           end,
    case ls_vm:incremental_backup(UUID, Parent, Comment, Opts) of
        {ok, BackupID} ->
            {ok, BackupID};
        _ ->
            {error, "failed!"}
    end;

call(_, Cmd) ->
    lager:warning("[api] Unsupported command: ~p", [Cmd]),
    {error, "unsupported"}.
