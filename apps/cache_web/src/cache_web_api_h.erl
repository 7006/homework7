-module(cache_web_api_h).

%% cowboy_rest callbacks
-export([init/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

%% internal
-export([next/2, next/3, next/4, next/5, next/6]).

%% cowboy_rest callbacks
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

known_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, next}], Req, State}.

%% internal
next(Req, State) ->
    next(has_req_body, Req, State).

next(has_req_body, Req, State) ->
    case cowboy_req:has_body(Req) of
        true ->
            next(read_req_body, Req, State);
        false ->
            next(error, empty_body, Req, State)
    end;
next(read_req_body, Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    next(cowboy, true, Body, Req1, State).

next(error, Code, Req, State) ->
    Status =
        case Code of
            _ -> 400
        end,
    Error = #{
        code => Code,
        status => Status
    },
    Body = jsone:encode(Error),
    next(cowboy, stop, Status, Body, Req, State).

next(cowboy, true, Body, Req, State) ->
    Req1 = cowboy_req:set_resp_headers(response_headers(), Req),
    Req2 = cowboy_req:set_resp_body(Body, Req1),
    {true, Req2, State}.

next(cowboy, stop, Status, Body, Req, State) ->
    Req1 = cowboy_req:reply(Status, response_headers(), Body, Req),
    {stop, Req1, State}.

response_headers() ->
    #{
        <<"content-type">> => <<"application/json">>
    }.
