-module(cache_ets_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_, _) ->
    cache_ets_sup:start_link().

stop(_) ->
    ok.
