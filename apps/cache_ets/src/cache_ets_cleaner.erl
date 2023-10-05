-module(cache_ets_cleaner).

-behaviour(gen_server).

-export([start/2]).
-export([start_link/2]).
-export([stop/1]).
-export([stats/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-record(state, {
    tab,
    cleanup_interval,
    cleanup_timer_ref,
    stats
}).

-define(now_utc, calendar:now_to_universal_time(erlang:timestamp())).

start(Opts, Tab) ->
    gen_server:start(?MODULE, [Opts, Tab], []).

start_link(Opts, Tab) ->
    gen_server:start_link(?MODULE, [Opts, Tab], []).

stop(Pid) ->
    gen_server:stop(Pid).

stats(Pid) ->
    gen_server:call(Pid, stats).

init([Opts, Tab]) ->
    S = #state{},
    S1 = init_stats(S),
    S2 = create_table(Tab, S1),
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
create_table(Tab, S) ->
    ets:new(Tab, [set, public, named_table]),
    S#state{tab = Tab}.

cleanup_table(S) ->
    _ = cache_ets_lib:delete_obsolete(S#state.tab).

schedule_table_cleanup_at_init(#{cleanup_interval := I}, S) ->
    Ref = erlang:send_after(I, self(), cleanup),
    S#state{cleanup_interval = I, cleanup_timer_ref = Ref}.

schedule_table_cleanup_at_info(S) ->
    _ = erlang:cancel_timer(S#state.cleanup_timer_ref),
    Ref = erlang:send_after(S#state.cleanup_interval, self(), cleanup),
    S#state{cleanup_timer_ref = Ref}.

cancel_table_cleanup_at_terminate(S) ->
    _ = erlang:cancel_timer(S#state.cleanup_timer_ref).

init_stats(S) ->
    Stats = #{
        run_at => 0,
        total_runs => 0
    },
    S#state{stats = Stats}.

update_stats(S) ->
    RunAt = ?now_utc,
    TotalRuns = maps:get(total_runs, S#state.stats),
    Stats = #{
        run_at => RunAt,
        total_runs => TotalRuns + 1
    },
    S#state{stats = Stats}.
