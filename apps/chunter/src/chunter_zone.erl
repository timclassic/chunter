%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 May 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_zone).

-define(ZONEADM, "/usr/sbin/zoneadm").


-export([list/0, get/1]).

-export([ex1/0]).


list() ->
    [chunter_zoneparser:load([{<<"name">>, Name}, {<<"uuid">>, UUID}]) ||
        %% SmartOS seems to have one more coumn
        [ID,Name,_VMState,_Path,UUID,_Type,_IP | _] <-
            [ re:split(Line, ":")
              || Line <- re:split(os:cmd("/usr/sbin/zoneadm list -ip"), "\n")],
        ID =/= <<"0">>].


get(ZUUID) ->
    case [chunter_zoneparser:load([{<<"name">>,Name},
                                   {<<"state">>, VMState},
                                   {<<"zonepath">>, Path},
                                   {<<"type">>, Type}]) ||
             {_ID, Name, VMState, Path, _UUID, Type} <- get1(ZUUID)] of
        [VM | _] ->
            VM;
        [] ->
            {error, not_found}
    end.

get1(UUID) when is_binary(UUID) ->
    Zones = [ re:split(Line, ":")
              || Line <- re:split(os:cmd([?ZONEADM, " -u ", UUID, " list -p"]), "\n")],
    [{ID, Name, VMState, Path, UUID1, Type} ||
        [ID, Name, VMState, Path, UUID1, Type, _IP | _] <- Zones].

%% create
%% set zonename=<uuid>
%% set uuid=<uuid>
%% set zonepath=/zones/<uuid>
%% set autoboot=true
%% set limitpriv=default,dtrace_proc,dtrace_user
%% set ip-type=exclusive
%% add net
%% set physical=myzone0
%% end
%% verify
%% commit
%% exit
ex1() ->
    zonecfg([create,
             {zonename, <<"uuid">>},
             {zonepath, <<"/zones/uuid">>},
             {autoboot, true},
             {limitpriv, [default,dtrace_proc,dtrace_user]},
             {'ip-type', exclusive},
             {add, net, [{physical, "myzone0"}]},
             verify,
             commit,
             exit]).

%% zonecfg -z 2398fe7c-032f-11e5-abb0-b33f9f953915 delete -F

%% zoneadm: zone '2398fe7c-032f-11e5-abb0-b33f9f953915': WARNING: Ignoring unrecognized rctl 'zone.max-physical-memory'.
%% zoneadm -z 2398fe7c-032f-11e5-abb0-b33f9f953915 install
%% zoneadm -z 2398fe7c-032f-11e5-abb0-b33f9f953915 boot

%% WARNING: skipping network interface 'net0': object not found
%% zone '2398fe7c-032f-11e5-abb0-b33f9f953915': WARNING: The zone.cpu-shares rctl is set but
%% zone '2398fe7c-032f-11e5-abb0-b33f9f953915': FSS is not the default scheduling class for
%% zone '2398fe7c-032f-11e5-abb0-b33f9f953915': this zone.  FSS will be used for processes
%% zone '2398fe7c-032f-11e5-abb0-b33f9f953915': in the zone but to get the full benefit of FSS,
%% zone '2398fe7c-032f-11e5-abb0-b33f9f953915': it should be the default scheduling class.
%% zone '2398fe7c-032f-11e5-abb0-b33f9f953915': See dispadmin(1M) for more details.
%% zone '2398fe7c-032f-11e5-abb0-b33f9f953915': failed to add network device: Error 0
%% zoneadm: zone '2398fe7c-032f-11e5-abb0-b33f9f953915': call to zoneadmd failed

zonecfg(L) ->
    [zonecfg1(C) || C <- L].

zonecfg1(create) ->
    "create\n";

zonecfg1(verify) ->
    "verify\n";

zonecfg1(commit) ->
    "commit\n";

zonecfg1(exit) ->
    "exit\n";

zonecfg1({N, V}) when is_atom(N) ->
    set(N, V);

zonecfg1({rctl, Name, Priv, Limit, Action}) ->
    add(rctl, [{name, Name},
               {value, rctl_val(Priv, Limit, Action)}]);

zonecfg1({add, What, Opts}) ->
    add(What, Opts);

zonecfg1({attr, Name, Type, Value}) ->
    add(attr,
        [{name, Name},
         {type, Type},
         {value, Value}]).

add(Type, Values) ->
    ["add ", v(Type),
     [set(K, V) || {K, V} <- Values],
     "end\n"].

set(Name, Value) ->
    ["set ", v(Name), "=", v(Value), $\n].

rctl_val(Priv, Limit, Action) ->
    val_list([{priv, Priv}, {limit, Limit}, {action, rctl_action(Action)}]).

val_list(Vs) ->
    [$(, v([[v(N), $=, v(V)] || {N, V} <- Vs]),  $)].

rctl_action(none) ->
    "none";
rctl_action(deny) ->
    "deny".

v(I) when is_integer(I) ->
    integer_to_list(I);

v(B) when is_binary(B) ->
    B;

v({list, L}) ->
    string:join([v(E) || E <- L], ",");

v([_E | _] = L) when not is_integer(_E) ->
    v({list, L});

v(L) when is_list(L) ->
    L;
v(A) when is_atom(A) ->
    atom_to_list(A).
