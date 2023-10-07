-module(cache_ets_time_lib).

-export([utc/1]).
-export([seconds/1]).

utc(now) ->
    calendar:universal_time().

seconds(now) ->
    seconds(utc(now));
seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).
