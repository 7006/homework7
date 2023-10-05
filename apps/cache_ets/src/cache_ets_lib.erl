-module(cache_ets_lib).

-include_lib("stdlib/include/ms_transform.hrl").

-export([insert/3, insert/4, insert/5]).
-export([lookup/2, lookup/3]).
-export([delete_obsolete/1, delete_obsolete/2]).

-define(now, calendar:datetime_to_gregorian_seconds(calendar:universal_time())).

insert(Tab, Key, Val) ->
    true = ets:insert(Tab, {Key, Val}).

insert(Tab, Key, Val, Ttl) when is_integer(Ttl), Ttl > 0 ->
    insert(Tab, Key, Val, Ttl, ?now).

insert(Tab, Key, Val, Ttl, Now) ->
    ExpiresAt = Now + Ttl,
    true = ets:insert(Tab, {Key, Val, ExpiresAt}).

lookup(Tab, Key) ->
    lookup(Tab, Key, ?now).

lookup(Tab, Key, Now) ->
    case ets:lookup(Tab, Key) of
        [{_, Val} | _] ->
            Val;
        [{_, Val, ExpiresAt} | _] when ExpiresAt >= Now ->
            Val;
        [{_, _, ExpiresAt} | _] when ExpiresAt < Now ->
            undefined;
        [] ->
            undefined
    end.

delete_obsolete(Tab) ->
    delete_obsolete(Tab, ?now).

delete_obsolete(Tab, Now) ->
    MatchSpec = ets:fun2ms(fun({_, _, ExpiresAt}) when ExpiresAt < Now -> true end),
    ets:select_delete(Tab, MatchSpec).
