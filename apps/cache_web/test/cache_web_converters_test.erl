-module(cache_web_converters_test).

-include_lib("eunit/include/eunit.hrl").

simple_to_datetime_test_() ->
    [
        ?_assertEqual(
            cache_web_converters:simple_to_datetime(<<"2015/1/1 00:00:00">>),
            {{2015, 1, 1}, {0, 0, 0}}
        ),
        ?_assertEqual(
            cache_web_converters:simple_to_datetime(<<"2015/1/10 23:59:59">>),
            {{2015, 1, 10}, {23, 59, 59}}
        ),
        ?_assertEqual(
            cache_web_converters:simple_to_datetime(<<"2015/01/1 00:00:00">>),
            {{2015, 1, 1}, {0, 0, 0}}
        ),
        ?_assertEqual(
            cache_web_converters:simple_to_datetime(<<"2015/01/10 23:59:59">>),
            {{2015, 1, 10}, {23, 59, 59}}
        ),
        ?_assertError(
            bad_simple_to_datetime,
            cache_web_converters:simple_to_datetime(<<"2015/01/1 100:00:00">>)
        ),
        ?_assertError(
            bad_simple_to_datetime,
            cache_web_converters:simple_to_datetime(<<"2015/01/10 23:61:59">>)
        ),
        ?_assertError(
            bad_simple_to_datetime,
            cache_web_converters:simple_to_datetime(<<"2015/01/10 23:00:99">>)
        )
    ].
