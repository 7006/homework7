-module(cache_web_converters).

-export([simple_to_datetime/1]).

simple_to_datetime(Bin) when is_binary(Bin) ->
    Parsed = ec_date:parse(binary:bin_to_list(Bin)),
    case valid_datetime(Parsed) of
        false ->
            error(bad_simple_to_datetime, [{binary, Bin}, {parsed, Parsed}]);
        true ->
            Parsed
    end.

valid_datetime({Date, Time}) ->
    calendar:valid_date(Date) andalso valid_time(Time).

valid_time({Hour, Minute, Second}) ->
    valid_hour(Hour) andalso valid_minute(Minute) andalso valid_second(Second).

valid_hour(Hour) ->
    Hour >= 0 andalso Hour =< 24.

valid_minute(Minute) ->
    Minute >= 0 andalso Minute =< 59.

valid_second(Second) ->
    Second >= 0 andalso Second =< 59.
