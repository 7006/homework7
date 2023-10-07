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
                "Insert",
                fun insert/0
            },
            {
                "Lookup",
                fun lookup/0
            },
            {
                "Lookup by date in 2015",
                fun lookup_by_date_in_2015/0
            },
            {
                "Lookup by date today",
                fun lookup_by_date_today/0
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

insert() ->
    post(#{
        body => #{
            <<"action">> => <<"insert">>,
            <<"key">> => <<"foobar">>,
            <<"value">> => [1, 2, 3]
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".result", <<"ok">>}
        ]
    }).

lookup() ->
    post(#{
        body => #{
            <<"action">> => <<"lookup">>,
            <<"key">> => <<"foobar">>
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".result", [1, 2, 3]}
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

lookup_by_date_today() ->
    Now = calendar:universal_time(),
    OneMinuteLater = begin
        {{Year, Month, Day}, {Hour, Minute, Second}} = Now,
        {{Year, Month, Day}, {Hour, Minute + 1, Second}}
    end,

    From = cache_web_converters:datetime_to_simple(Now),
    To = cache_web_converters:datetime_to_simple(OneMinuteLater),

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
                #{<<"key">> => <<"foobar">>, <<"value">> => [1, 2, 3]}
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
