-module(cache_web_api_action).

-export([call/1]).

call(#{<<"action">> := <<"insert">>, <<"key">> := Key, <<"value">> := Value}) ->
    DataOut = #{
        <<"result">> => <<"ok">>
    },
    {ok, DataOut};
call(#{<<"action">> := <<"lookup">>, <<"key">> := Key}) ->
    DataOut = #{
        <<"result">> => [1, 2, 3]
    },
    {ok, DataOut};
call(#{<<"action">> := <<"lookup_by_date">>, <<"date_from">> := From, <<"date_to">> := To}) ->
    DataOut = #{
        <<"result">> => [
            #{
                <<"key">> => <<"some_key">>,
                <<"value">> => [1, 2, 3]
            }
        ]
    },
    {ok, DataOut};
call(#{<<"action">> := <<"echo">>} = X) ->
    {ok, X};
call(_) ->
    {error, unknown_action}.
