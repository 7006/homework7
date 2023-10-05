-module(cache_web_api_test).

-include_lib("eunit/include/eunit.hrl").

cache_web_api_test_() ->
    {
        setup,
        fun setup/0,
        fun cleanup/1,
        [
            {
                "Echo",
                fun echo_req/0
            },
            {
                "Insert object",
                fun insert_req/0
            },
            {
                "Lookup object by key",
                fun lookup_req/0
            },
            {
                "Lookup objects by date range",
                fun lookup_by_date_req/0
            },
            {
                "Send Empty body",
                fun send_empty_body/0
            }
        ]
    }.

setup() ->
    application:set_env(cache_web, port, 80),
    application:ensure_all_started(cache_web),
    application:ensure_all_started(efrisby),
    ok.

cleanup(_) ->
    application:stop(cache_web),
    application:stop(efrisby),
    ok.

echo_req() ->
    post(#{
        body => #{
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

insert_req() ->
    post(#{
        body => #{
            <<"action">> => <<"insert">>,
            <<"key">> => <<"some_key">>,
            <<"value">> => [1, 2, 3]
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".action", <<"insert">>}
        ]
    }).

lookup_req() ->
    post(#{
        body => #{
            <<"action">> => <<"lookup">>,
            <<"key">> => <<"some_key">>
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".action", <<"lookup">>}
        ]
    }).

lookup_by_date_req() ->
    post(#{
        body => #{
            <<"action">> => <<"lookup_by_date">>,
            <<"date_from">> => <<"2015/1/1 00:00:00">>,
            <<"date_to">> => <<"2015/1/10 23:59:59">>
        },
        expect => [
            {status, 200},
            {content_type, <<"application/json">>},
            {json, ".action", <<"lookup_by_date">>}
        ]
    }).

send_empty_body() ->
    post(#{
        body => null,
        expect => [
            {status, 400},
            {content_type, <<"application/json">>},
            {json, ".code", <<"empty_body">>},
            {json, ".status", 400}
        ]
    }).

post(#{body := Body, expect := Expect}) ->
    Opts = [
        {base_url, "http://localhost/api/cache_server"},
        {headers, [
            {content_type, <<"application/json">>}
        ]}
    ],
    {ok, _} = efrisby:post("/", Body, Expect, Opts).
