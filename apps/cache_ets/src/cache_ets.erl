-module(cache_ets).

-export([start_cache_worker/1, stop_cache_worker/1]).
-export([insert/3, insert/4]).
-export([lookup/2]).
-export([lookup_by_date/3]).

start_cache_worker(Opts) ->
    cache_ets_sup:start_cache_worker(Opts).

stop_cache_worker(Pid) ->
    cache_ets_sup:stop_cache_worker(Pid).

insert(TableName, Key, Val) ->
    cache_ets_table:insert(TableName, Key, Val).

insert(TableName, Key, Val, Ttl) ->
    cache_ets_table:insert(TableName, Key, Val, Ttl).

lookup(TableName, Key) ->
    cache_ets_table:lookup(TableName, Key).

lookup_by_date(TableName, From, To) ->
    cache_ets_table:lookup_by_date(TableName, From, To).
