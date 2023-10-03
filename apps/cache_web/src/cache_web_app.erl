%%%-------------------------------------------------------------------
%% @doc cache_web public API
%% @end
%%%-------------------------------------------------------------------

-module(cache_web_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cache_web_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
