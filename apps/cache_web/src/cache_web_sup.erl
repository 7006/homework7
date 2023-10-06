-module(cache_web_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => cache_ets_sup,
            start => {cache_ets_sup, start_link, []},
            type => supervisor
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
