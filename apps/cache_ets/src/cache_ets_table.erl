-module(cache_ets_table).

-include_lib("stdlib/include/ms_transform.hrl").

-export([insert/3, insert/4, insert/5]).
-export([lookup/2, lookup/3]).
-export([lookup_by_date/3]).
-export([delete_expired/1, delete_expired/2]).

insert(Tab, Key, Val) ->
    Now = cache_ets_time_lib:seconds(now),
    CreatedAt = Now,
    ExpiresAt = inf,
    insert(Tab, Key, Val, CreatedAt, ExpiresAt).

insert(Tab, Key, Val, Ttl) when is_integer(Ttl), Ttl > 0 ->
    Now = cache_ets_time_lib:seconds(now),
    CreatedAt = Now,
    ExpiresAt = Now + Ttl,
    insert(Tab, Key, Val, CreatedAt, ExpiresAt).

insert(Tab, Key, Val, CreatedAt, ExpiresAt) ->
    true = ets:insert(Tab, {Key, Val, CreatedAt, ExpiresAt}).

lookup(Tab, Key) ->
    Now = cache_ets_time_lib:seconds(now),
    lookup(Tab, Key, Now).

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
    Now = cache_ets_time_lib:seconds(now),
    lookup_by_date(Tab, FromDateTime, ToDateTime, Now).

lookup_by_date(Tab, FromDateTime, ToDateTime, Now) ->
    From = cache_ets_time_lib:seconds(FromDateTime),
    To = cache_ets_time_lib:seconds(ToDateTime),
    MatchSpec = ets:fun2ms(
        fun(
            {
                Key,
                Val,
                CreatedAt,
                ExpiresAt
            }
        ) when From =< CreatedAt, CreatedAt =< To, ExpiresAt >= Now ->
            {Key, Val}
        end
    ),

    %% On Erlang/OTP 24 I've got this error
    %% "the language element map (in body) cannot be translated into match_spec"
    %% but on Erlang/OTP 25 I can use maps in body of the match spec function
    lists:map(
        fun({Key, Value}) ->
            #{
                key => Key,
                value => Value
            }
        end,
        ets:select(Tab, MatchSpec)
    ).

delete_expired(Tab) ->
    Now = cache_ets_time_lib:seconds(now),
    delete_expired(Tab, Now).

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
