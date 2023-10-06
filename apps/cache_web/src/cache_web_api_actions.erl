-module(cache_web_api_actions).

-export([run_action/2]).

run_action(Name, #{
    <<"action">> := <<"insert">>,
    <<"key">> := Key,
    <<"value">> := Value
}) ->
    insert(Name, Key, Value);
run_action(Name, #{
    <<"action">> := <<"lookup">>,
    <<"key">> := Key
}) ->
    lookup(Name, Key);
run_action(Name, #{
    <<"action">> := <<"lookup_by_date">>,
    <<"date_from">> := From,
    <<"date_to">> := To
}) ->
    lookup_by_date(Name, From, To);
run_action(_, DataIn) when map_get(<<"action">>, DataIn) =:= <<"echo">> ->
    {ok, DataIn};
run_action(_, _) ->
    {error, unknown_action}.

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
