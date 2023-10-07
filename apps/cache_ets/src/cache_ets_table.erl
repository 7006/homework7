-module(cache_ets_table).

-include_lib("stdlib/include/ms_transform.hrl").

-export([insert/3, insert/4, insert/5]).
-export([lookup/2, lookup/3]).
-export([lookup_by_date/3]).
-export([delete_expired/1, delete_expired/2]).

-define(seconds(DateTime), calendar:datetime_to_gregorian_seconds(DateTime)).
-define(now, calendar:datetime_to_gregorian_seconds(calendar:universal_time())).

insert(Tab, Key, Val) ->
    insert(Tab, Key, Val, ?now, inf).

insert(Tab, Key, Val, Ttl) when is_integer(Ttl), Ttl > 0 ->
    insert(Tab, Key, Val, ?now, ?now + Ttl).

insert(Tab, Key, Val, CreatedAt, ExpiresAt) ->
    true = ets:insert(Tab, {Key, Val, CreatedAt, ExpiresAt}).

lookup(Tab, Key) ->
    lookup(Tab, Key, ?now).

lookup(Tab, Key, Now) ->
    case ets:lookup(Tab, Key) of
        [] ->
            undefined;
        [{_, _, _, ExpiresAt} | _] when ExpiresAt < Now ->
            undefined;
        [{_, Val, _, _} | _] ->
            Val
    end.

lookup_by_date(Tab, FromDateTime, ToDateTime) ->
    lookup_by_date(Tab, FromDateTime, ToDateTime, ?now).

lookup_by_date(Tab, FromDateTime, ToDateTime, Now) ->
    From = ?seconds(FromDateTime),
    To = ?seconds(ToDateTime),
    MatchSpec = ets:fun2ms(
        fun(
            {
                Key,
                Val,
                CreatedAt,
                ExpiresAt
            }
        ) when From =< CreatedAt, CreatedAt =< To, ExpiresAt >= Now ->
            #{key => Key, value => Val}
        end
    ),
    ets:select(Tab, MatchSpec).

delete_expired(Tab) ->
    delete_expired(Tab, ?now).

delete_expired(Tab, Now) ->
    MatchSpec = ets:fun2ms(
        fun(
            {
                Key,
                Val,
                CreatedAt,
                ExpiresAt
            }
        ) when ExpiresAt < Now ->
            true
        end
    ),
    ets:select_delete(Tab, MatchSpec).
