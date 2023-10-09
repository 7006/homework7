-module(cache_web_app).

-behaviour(application).

-export([start/2]).
-export([start_phase/3]).
-export([stop/1]).

-define(LISTENER_NAME, ?MODULE).

start(_, _) ->
    cache_web_sup:start_link().

start_phase(start_cowboy_listener, _, _) ->
    Port = application:get_env(cache_web, port, 80),
    TransportOpts = [
        {port, Port}
    ],

    BaseUrl = application:get_env(cache_web, base_url, "/"),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", readyz_h, []},
            {BaseUrl, cache_web_api_h, []}
        ]}
    ]),
    ProtocolOpts = #{
        env => #{
            dispatch => Dispatch
        }
    },

    case cowboy:start_clear(?LISTENER_NAME, TransportOpts, ProtocolOpts) of
        {error, Reason} ->
            {error, Reason};
        {ok, _} ->
            ok
    end;
start_phase(start_cache_worker, _, _) ->
    {ok, WorkerOpts} = application:get_env(cache_web, cache_worker_opts),
    case cache_ets:start_cache_worker(WorkerOpts) of
        {error, Reason} ->
            {error, Reason};
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok
    end.

stop(_) ->
    ok = cowboy:stop_listener(?LISTENER_NAME).
