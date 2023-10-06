-module(cache_web_api_actions).

-export([route_action/2]).
-export([apply_action/2]).
-export([insert/3]).
-export([lookup/2]).
-export([lookup_by_date/3]).
-export([echo/2]).

route_action(Name, #{
    <<"action">> := <<"insert">>,
    <<"key">> := Key,
    <<"value">> := Value
}) ->
    apply_action(fun insert/3, [Name, Key, Value]);
route_action(Name, #{
    <<"action">> := <<"lookup">>,
    <<"key">> := Key
}) ->
    apply_action(fun lookup/2, [Name, Key]);
route_action(Name, #{
    <<"action">> := <<"lookup_by_date">>,
    <<"date_from">> := From,
    <<"date_to">> := To
}) ->
    apply_action(fun lookup_by_date/3, [Name, From, To]);
route_action(Name, Echo) when map_get(<<"action">>, Echo) =:= <<"echo">> ->
    apply_action(fun echo/2, [Name, Echo]);
route_action(_, _) ->
    {error, unknown_action}.

apply_action(Action, Args) ->
    case erlang:apply(Action, Args) of
        {ok, R} ->
            {ok, R};
        {error, Error} ->
            {error, Error}
    end.

insert(Name, Key, Value) ->
    cache_ets:insert(Name, Key, Value),
    {ok, #{
        <<"result">> => <<"ok">>
    }}.

lookup(Name, Key) ->
    Result = cache_ets:lookup(Name, Key),
    {ok, #{
        <<"result">> => Result
    }}.

lookup_by_date(Name, From, To) ->
    Result = cache_ets:lookup_by_date(Name, From, To),
    {ok, #{
        <<"result">> => Result
    }}.

echo(Name, Echo) ->
    R = maps:merge(#{cache_name => Name}, Echo),
    {ok, R}.
