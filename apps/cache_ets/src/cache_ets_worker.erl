-module(cache_ets_worker).

-behaviour(gen_server).

-export([start_link/1]).
-export([stop/1]).
-export([stats/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-record(state, {
    table_name,
    cleanup_interval,
    cleanup_timer_ref,
    stats
}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

stop(Pid) ->
    gen_server:stop(Pid).

stats(Pid) ->
    gen_server:call(Pid, stats).

init(Opts) ->
    S = #state{},
    S1 = init_stats(S),
    S2 = create_table(Opts, S1),
    S3 = schedule_table_cleanup_at_init(Opts, S2),
    {ok, S3}.

terminate(_, S) ->
    cancel_table_cleanup_at_terminate(S),
    ok.

handle_call(stats, _, S) ->
    {reply, S#state.stats, S};
handle_call(_, _, S) ->
    {reply, ignored, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info(cleanup, S) ->
    cleanup_table(S),
    S1 = update_stats(S),
    S2 = schedule_table_cleanup_at_info(S1),
    {noreply, S2};
handle_info(_, S) ->
    {noreply, S}.

%% internal
create_table(Opts, S) ->
    TableName = proplists:get_value(table_name, Opts),
    ets:new(TableName, [set, public, named_table]),
    S#state{table_name = TableName}.

cleanup_table(S) ->
    _ = cache_ets_table:delete_expired(S#state.table_name).

schedule_table_cleanup_at_init(Opts, S) ->
    CleanupInterval = proplists:get_value(cleanup_interval, Opts),
    CleanupTimerRef = erlang:send_after(CleanupInterval, self(), cleanup),
    S#state{
        cleanup_interval = CleanupInterval,
        cleanup_timer_ref = CleanupTimerRef
    }.

schedule_table_cleanup_at_info(S) ->
    _ = erlang:cancel_timer(S#state.cleanup_timer_ref),
    CleanupTimerRef = erlang:send_after(S#state.cleanup_interval, self(), cleanup),
    S#state{cleanup_timer_ref = CleanupTimerRef}.

cancel_table_cleanup_at_terminate(S) ->
    _ = erlang:cancel_timer(S#state.cleanup_timer_ref).

init_stats(S) ->
    Stats = #{
        run_at => 0,
        total_runs => 0
    },
    S#state{stats = Stats}.

update_stats(S) ->
    RunAt = cache_ets_time_lib:utc(now),
    TotalRuns = maps:get(total_runs, S#state.stats),
    Stats = #{
        run_at => RunAt,
        total_runs => TotalRuns + 1
    },
    S#state{stats = Stats}.
