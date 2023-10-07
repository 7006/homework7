-module(cache_web_converters).

-export([simple_to_datetime/1]).

simple_to_datetime(Simple) when is_binary(Simple) ->
    DateTime = ec_date:parse(binary:bin_to_list(Simple)),
    case valid_datetime(DateTime) of
        false ->
            error(bad_simple_to_datetime, [{simple, Simple}, {datetime, DateTime}]);
        true ->
            DateTime
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
