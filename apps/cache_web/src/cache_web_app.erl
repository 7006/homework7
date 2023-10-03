-module(cache_web_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    start_cowboy_listener(),
    cache_web_sup:start_link().

stop(_) ->
    stop_cowboy_listener().

start_cowboy_listener() ->
    {ok, Port} = application:get_env(cache_web, port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/cache_server", cache_web_api_h, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        ?MODULE,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).

stop_cowboy_listener() ->
    ok = cowboy:stop_listener(?MODULE).
