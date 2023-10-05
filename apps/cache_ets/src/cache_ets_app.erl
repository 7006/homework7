-module(cache_ets_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    Opts = cache_ets_config:get_options(),
    cache_ets_sup:start_link(Opts).

stop(_) ->
    ok.
