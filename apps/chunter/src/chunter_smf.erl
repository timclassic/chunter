-module(chunter_smf).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([update/1, update/2]).

update(Old) ->
    update(<<"global">>, Old).

update(UUID, OldServices) ->
    R = case UUID of
            <<"global">> ->
                smurf:list();
            _ ->
                smurf:list(UUID)
        end,
    case R of
        {ok, Services} ->
            ServiceSet = ordsets:from_list(Services),
            case cmp_states(OldServices, ServiceSet) of
                [] ->
                    {ok, ServiceSet, []};
                Changed ->
                    {ok, ServiceSet, Changed}
            end;
        E ->
            E
    end.

cmp_states(Old, New) ->
    Changed1 = lists:foldl(fun({Srv, State, _}, Acc) ->
                                   case lists:keyfind(Srv, 1, Old) of
                                       false ->
                                           [{Srv, <<"none">>, State} | Acc];
                                       {Srv, OldState, _} ->
                                           [{Srv, OldState, State} | Acc]
                                   end
                           end, [], ordsets:subtract(New, Old)),
    Changed2 = lists:foldl(fun({Srv, State, _}, Acc) ->
                                   case lists:keyfind(Srv, 1, New) of
                                       false ->
                                           [{Srv, State, <<"removed">>} | Acc];
                                       {Srv, _, _} ->
                                           Acc
                                   end
                           end, Changed1, ordsets:subtract(Old, New)),
    ordsets:from_list(Changed2).

-ifdef(TEST).

cmp_states_test() ->
    Old = [{unchanged, running, 0},
           {unchanged1, running, 0},
           {deleted, running, 0},
           {changed, stopped, 0}],
    New = [{unchanged, running, 0},
           {unchanged1, running, 0},
           {new, running, 0},
           {changed, running, 0}],
    OldS = ordsets:from_list(Old),
    NewS = ordsets:from_list(New),
    Expected = [{deleted, running, <<"removed">>},
                {new, <<"none">>, running},
                {changed, stopped, running}],
    ExpectedS = ordsets:from_list(Expected),
    ?assertEqual(ExpectedS, cmp_states(OldS, NewS)).

-endif.
