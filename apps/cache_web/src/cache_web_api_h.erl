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
    {[{{<<"application">>, <<"json">>, '*'}, handle_request_stop}], Req, State}.

handle_request_true(Req, State) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req),
    Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req1),
    Req3 = cowboy_req:set_resp_body(Data, Req2),
    {true, Req3, State}.

handle_request_stop(Req, State) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Data, Req1),
    {stop, Req2, State}.
