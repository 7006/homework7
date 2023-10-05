-module(cache_ets_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/1]).
-export([start_cache_cleaner/1]).

init(Opts) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 5
    },
    ChildSpec = #{
        id => cache_cleaner,
        start => {cache_ets_cleaner, start_link, [Opts]},
        type => worker
    },
    {ok, {SupFlags, [ChildSpec]}}.

start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

start_cache_cleaner(Tab) ->
    supervisor:start_child(?MODULE, [Tab]).
