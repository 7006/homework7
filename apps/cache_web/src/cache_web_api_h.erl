-module(cache_web_api_h).

-export([init/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

-export([handle_request_stop/2, handle_request_true/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

known_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_request_true}], Req, State}.

handle_request_true(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
    Req2 = cowboy_req:set_resp_body(<<"[]">>, Req1),
    {true, Req2, State}.

handle_request_stop(Req, State) ->
    Req2 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"application/json">>},
        <<"[]">>,
        Req
    ),
    {stop, Req2, State}.
