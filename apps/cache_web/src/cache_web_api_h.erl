-module(cache_web_api_h).

%% cowboy_rest callbacks
-export([init/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

%% internal
-export([enter/2]).
-export([has_req_body/2]).
-export([read_req_body/2]).
-export([parse_req_body/3]).
-export([get_cache_name/3]).
-export([route_action/4]).
-export([encode_action_data/3]).
-export([handle_req_error/3]).
-export([handle_req_error/4]).
-export([reply/3]).
-export([reply_error/4]).
-export([error_status/1]).
-export([error_body/2]).
-export([error_body/3]).
-export([response_headers/0]).

%% cowboy_rest callbacks
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

known_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, enter}], Req, State}.

%% internal
enter(Req, State) ->
    has_req_body(Req, State).

has_req_body(Req, State) ->
    case cowboy_req:has_body(Req) of
        true ->
            read_req_body(Req, State);
        false ->
            handle_req_error(empty_body, Req, State)
    end.

read_req_body(Req, State) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req1} ->
            parse_req_body(Body, Req1, State);
        {more, _, Req1} ->
            handle_req_error(stream_body, Req1, State)
    end.

parse_req_body(Body, Req, State) ->
    try jsone:decode(Body) of
        DataIn ->
            get_cache_name(DataIn, Req, State)
    catch
        error:Error:_ ->
            handle_req_error(bad_json, Error, Req, State)
    end.

get_cache_name(DataIn, Req, State) ->
    case application:get_env(cache_web, cache_name) of
        {ok, undefined} ->
            {error, bad_cache_name};
        {ok, Name} ->
            route_action(DataIn, Name, Req, State)
    end.

route_action(DataIn, Name, Req, State) ->
    case cache_web_api_actions:route_action(Name, DataIn) of
        {ok, DataOut} ->
            encode_action_data(DataOut, Req, State);
        {error, Error} ->
            handle_req_error(bad_action, Error, Req, State)
    end.

encode_action_data(DataOut, Req, State) ->
    try
        Body = jsone:encode(DataOut),
        reply(Body, Req, State)
    catch
        error:Error:_ ->
            handle_req_error(bad_action_data_out, Error, Req, State)
    end.

handle_req_error(Code, Req, State) ->
    Status = error_status(Code),
    Body = jsone:encode(error_body(Code, Status)),
    reply_error(Status, Body, Req, State).

handle_req_error(Code, Error, Req, State) ->
    Status = error_status(Code),
    Body = jsone:encode(error_body(Code, Error, Status)),
    reply_error(Status, Body, Req, State).

reply(Body, Req, State) ->
    Req1 = cowboy_req:set_resp_headers(response_headers(), Req),
    Req2 = cowboy_req:set_resp_body(Body, Req1),
    {true, Req2, State}.

reply_error(Status, Body, Req, State) ->
    Req1 = cowboy_req:reply(Status, response_headers(), Body, Req),
    {stop, Req1, State}.

error_status(_Code) ->
    400.

error_body(Code, Status) ->
    #{
        code => Code,
        status => Status
    }.

error_body(Code, Error, Status) ->
    #{
        code => Code,
        status => Status,
        error => Error
    }.

response_headers() ->
    #{
        <<"content-type">> => <<"application/json">>
    }.
