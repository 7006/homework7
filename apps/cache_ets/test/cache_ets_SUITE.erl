-module(cache_ets_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([insert_and_lookup/1]).
-export([lookup_non_existent/1]).
-export([lookup_expired/1]).
-export([initial_stats/1]).
-export([running_stats/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        insert_and_lookup,
        lookup_non_existent,
        lookup_expired,
        initial_stats,
        running_stats
    ].

-define(one_second, 1_000).
-define(two_second, 2_000).
-define(ten_second, 10_000).

init_per_suite(Config) ->
    application:start(cache_ets),
    Config.

end_per_suite(_) ->
    application:stop(cache_ets).

init_per_testcase(lookup_expired, Config) ->
    {ok, Pid} =
        cache_ets:start_cache_worker([
            {table_name, my_cache},
            {cleanup_interval, ?two_second}
        ]),
    [{pid, Pid} | Config];
init_per_testcase(running_stats, Config) ->
    {ok, Pid} =
        cache_ets:start_cache_worker([
            {table_name, my_cache},
            {cleanup_interval, ?ten_second}
        ]),
    [{pid, Pid} | Config];
init_per_testcase(_, Config) ->
    {ok, Pid} =
        cache_ets:start_cache_worker([
            {table_name, my_cache}
        ]),
    [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
    Pid = ?config(pid, Config),
    cache_ets:stop_cache_worker(Pid).

insert_and_lookup(_) ->
    cache_ets:insert(my_cache, k1, v1),
    cache_ets:insert(my_cache, k2, v2),
    v1 = cache_ets:lookup(my_cache, k1),
    v2 = cache_ets:lookup(my_cache, k2).

lookup_non_existent(_) ->
    cache_ets:insert(my_cache, k, v),
    undefined = cache_ets:lookup(my_cache, x).

lookup_expired(_) ->
    cache_ets:insert(my_cache, k, v, 1),
    timer:sleep(?two_second),
    undefined = cache_ets:lookup(my_cache, k).

initial_stats(Config) ->
    Pid = ?config(pid, Config),
    #{run_at := 0, total_runs := 0} = cache_ets_worker:stats(Pid).

running_stats(Config) ->
    Pid = ?config(pid, Config),

    timer:sleep(?one_second),
    Pid ! cleanup,
    #{
        run_at := {
            {Year, Month, Day},
            {Hour, Minute, Second1}
        },
        total_runs := TotalRuns1
    } = cache_ets_worker:stats(Pid),

    timer:sleep(?one_second),
    Pid ! cleanup,
    #{
        run_at := {
            {Year, Month, Day},
            {Hour, Minute, Second2}
        },
        total_runs := TotalRuns2
    } = cache_ets_worker:stats(Pid),

    true = Second2 > Second1,
    1 = TotalRuns2 - TotalRuns1.
