-module(cache_web_api_test).

-include_lib("eunit/include/eunit.hrl").

cache_web_api_test_() ->
    {
        setup,
        fun setup/0,
        fun cleanup/1,
        [
            {
                "Empty",
                fun empty/0
            },
            {
                "Bad json",
                fun bad_json/0
            },
            {
                "Echo",
                fun echo/0
            },
            {
                "Insert and lookup",
                fun insert_and_lookup/0
            },
            {
                "Lookup by date in 2015",
                fun lookup_by_date_in_2015/0
            },
            {
                "Insert and lookup by date today",
                fun insert_and_lookup_by_date_today/0
            }
        ]
    }.

setup() ->
    application:ensure_all_started(cache_web),
    application:ensure_all_started(efrisby),
    ok.

cleanup(_) ->
    application:stop(efrisby),
    application:stop(cache_web),
    ok.

empty() ->
    post(#{
        body => null,
        expect => [
            {status, 400},
            {content_type, <<"application/json">>},
            {json, ".code", <<"empty_body">>},
            {json, ".status", 400}
        ]
    }).

bad_json() ->
    post(#{
        body => <<"[broken : json, ">>,
        expect => [
            {status, 400},
            {content_type, <<"application/json">>},
            {json, ".code", <<"bad_json">>},
            {json, ".status", 400}
        ]
    }).

echo() ->
    post(#{
        body => #{
            <<"action">> => <<"echo">>,
            <<"a">> => #{
                <<"b">> => #{
                    <<"c">> => 1
                }
            }
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".a.b.c", 1}
        ]
    }).

insert_and_lookup() ->
    Key = <<"foobar">>,
    Value = [1, 2, 3],

    post(#{
        body => #{
            <<"action">> => <<"insert">>,
            <<"key">> => Key,
            <<"value">> => Value
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".result", <<"ok">>}
        ]
    }),
    post(#{
        body => #{
            <<"action">> => <<"lookup">>,
            <<"key">> => Key
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".result", Value}
        ]
    }).

lookup_by_date_in_2015() ->
    post(#{
        body => #{
            <<"action">> => <<"lookup_by_date">>,
            <<"date_from">> => <<"2015/1/1 00:00:00">>,
            <<"date_to">> => <<"2015/1/10 23:59:59">>
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".result", []}
        ]
    }).

insert_and_lookup_by_date_today() ->
    Key = <<"k">>,
    Value = <<"v">>,
    Now = calendar:universal_time(),
    OneMinuteLater = begin
        {{Year, Month, Day}, {Hour, Minute, Second}} = Now,
        {{Year, Month, Day}, {Hour, Minute + 1, Second}}
    end,
    From = cache_web_converters:datetime_to_simple(Now),
    To = cache_web_converters:datetime_to_simple(OneMinuteLater),

    post(#{
        body => #{
            <<"action">> => <<"insert">>,
            <<"key">> => Key,
            <<"value">> => Value
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".result", <<"ok">>}
        ]
    }),
    post(#{
        body => #{
            <<"action">> => <<"lookup_by_date">>,
            <<"date_from">> => From,
            <<"date_to">> => To
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".result", [
                {Key, Value}
            ]}
        ]
    }).

post(#{body := Body, expect := Expect}) ->
    Opts = [
        {base_url, "http://localhost"},
        {headers, [
            {content_type, <<"application/json">>}
        ]}
    ],
    {ok, _} = efrisby:post("/", Body, Expect, Opts).
