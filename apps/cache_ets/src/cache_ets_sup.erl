-module(cache_ets_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_cache_worker/1, stop_cache_worker/1]).

-export([init/1]).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 5
    },
    ChildSpecs = [
        #{
            id => cache_worker,
            start => {cache_ets_worker, start_link, []},
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_cache_worker(Opts) ->
    TableName =
        case proplists:is_defined(table_name, Opts) of
            true ->
                proplists:get_value(table_name, Opts);
            false ->
                error(badarg, table_name)
        end,

    CleanupInterval =
        case proplists:is_defined(cleanup_interval, Opts) of
            true ->
                proplists:get_value(cleanup_interval, Opts);
            false ->
                case application:get_env(cache_ets, cleanup_interval) of
                    {ok, AppDefaultCleanupInterval} ->
                        AppDefaultCleanupInterval;
                    undefined ->
                        error(badarg, cleanup_interval)
                end
        end,

    WorkerOpts = [
        [
            {table_name, TableName},
            {cleanup_interval, CleanupInterval}
        ]
    ],

    supervisor:start_child(?MODULE, WorkerOpts).

stop_cache_worker(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).
