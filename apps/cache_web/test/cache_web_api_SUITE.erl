-module(cache_web_api_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([empty_body/1]).
-export([bad_json/1]).
-export([echo/1]).
-export([insert/1]).
-export([lookup/1]).
-export([lookup_by_date_in_2015/1]).
-export([lookup_by_date_today/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        empty_body,
        bad_json,
        echo,
        insert,
        lookup,
        lookup_by_date_in_2015,
        lookup_by_date_today
    ].

init_per_suite(Config) ->
    application:ensure_all_started(cache_web),
    application:ensure_all_started(efrisby),

    {ok, Port} = application:get_env(cache_web, port),
    {ok, BaseUrl} = application:get_env(cache_web, base_url),

    EfrisbyBaseUrl = string:join(
        ["http://localhost:", erlang:integer_to_list(Port), BaseUrl],
        ""
    ),
    EfrisbyOpts = [
        {base_url, EfrisbyBaseUrl},
        {headers, [
            {content_type, <<"application/json">>}
        ]}
    ],

    [{efrisby_opts, EfrisbyOpts} | Config].

end_per_suite(_) ->
    application:stop(efrisby),
    application:stop(cache_web).

empty_body(Config) ->
    post(
        #{
            body => null,
            expect => [
                {status, 400},
                {content_type, <<"application/json">>},
                {json, ".code", <<"empty_body">>},
                {json, ".status", 400}
            ]
        },
        ?config(efrisby_opts, Config)
    ).

bad_json(Config) ->
    post(
        #{
            body => <<"[broken : json, ">>,
            expect => [
                {status, 400},
                {content_type, <<"application/json">>},
                {json, ".code", <<"bad_json">>},
                {json, ".status", 400}
            ]
        },
        ?config(efrisby_opts, Config)
    ).

echo(Config) ->
    post(
        #{
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
        },
        ?config(efrisby_opts, Config)
    ).

insert(Config) ->
    post(
        #{
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
        },
        ?config(efrisby_opts, Config)
    ).

lookup(Config) ->
    post(
        #{
            body => #{
                <<"action">> => <<"lookup">>,
                <<"key">> => <<"foobar">>
            },
            expect => [
                {status, 200},
                {content_type, <<"application/json">>},
                {json, ".result", [1, 2, 3]}
            ]
        },
        ?config(efrisby_opts, Config)
    ).

lookup_by_date_in_2015(Config) ->
    post(
        #{
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
        },
        ?config(efrisby_opts, Config)
    ).

lookup_by_date_today(Config) ->
    Now = calendar:universal_time(),
    OneMinuteLater = begin
        {{Year, Month, Day}, {Hour, Minute, Second}} = Now,
        {{Year, Month, Day}, {Hour, Minute + 1, Second}}
    end,

    From = cache_web_converters:datetime_to_simple(Now),
    To = cache_web_converters:datetime_to_simple(OneMinuteLater),

    post(
        #{
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
        },
        ?config(efrisby_opts, Config)
    ).

%% internal

post(#{body := Body, expect := Expect}, Opts) ->
    {ok, _} = efrisby:post("/", Body, Expect, Opts).
