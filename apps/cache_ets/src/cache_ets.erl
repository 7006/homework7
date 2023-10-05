-module(cache_ets).

-export([create/1]).
-export([insert/3, insert/4]).
-export([lookup/2]).

create(Tab) ->
    {ok, _} = cache_ets_sup:start_cache_cleaner(Tab),
    ok.

insert(Tab, Key, Val) ->
    cache_ets_lib:insert(Tab, Key, Val),
    ok.

insert(Tab, Key, Val, Ttl) ->
    cache_ets_lib:insert(Tab, Key, Val, Ttl),
    ok.

lookup(Tab, Key) ->
    cache_ets_lib:lookup(Tab, Key).
