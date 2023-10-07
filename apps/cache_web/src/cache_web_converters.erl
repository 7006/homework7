-module(cache_web_converters).

-export([datetime_to_simple/1]).
-export([simple_to_datetime/1]).

-define(simple_format, "Y/n/j H:i:s").

datetime_to_simple(DateTime) ->
    case valid_datetime(DateTime) of
        false ->
            error(invalid_datetime_to_simple, [{datetime, DateTime}]);
        true ->
            binary:list_to_bin(ec_date:format(?simple_format, DateTime))
    end.

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
