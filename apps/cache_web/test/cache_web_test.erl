-module(cache_web_test).

-include_lib("eunit/include/eunit.hrl").

-define(_assertStatus(Req, Status),
    ?_assertNotEqual(
        nomatch,
        binary:match(
            erlang:list_to_binary(
                ?cmd(
                    <<"curl http://localhost/api/cache_server", " --verbose", " --request POST",
                        " --header \"Content-Type: application/json\"",
                        " --data @apps/cache_web/test/requests/",
                        (erlang:atom_to_binary(Req))/binary, ".json">>
                )
            ),
            <<"HTTP/1.1 ", (erlang:integer_to_binary(Status))/binary>>
        )
    )
).

-define(OPTS, [
    {base_url, "http://localhost/api/cache_server"},
    {headers, [
        {content_type, <<"application/json">>}
    ]}
]).

cache_web_test_() ->
    {
        setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Insert", ?_assertStatus(insert, 200)},
            {"Lookup", ?_assertStatus(lookup, 200)},
            {"Lookup By Date", ?_assertStatus(lookup_by_date, 200)},
            {"Insert", fun insert_req/0},
            {"Lookup", fun lookup_req/0},
            {"Lookup By Date", fun lookup_by_date_req/0}
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

insert_req() ->
    Body = #{
        <<"Key">> => <<"Value">>
    },

    Exp = [
        {status, 200},
        {content_type, <<"application/json">>},
        {
            json_types,
            ".",
            [
                {<<"Key">>, bitstring},
                {<<"Value">>, bitstring}
            ]
        },
        {
            json,
            ".",
            [
                {<<"Key">>, <<"Value">>}
            ]
        }
    ],

    {ok, _} = efrisby:post("/", Body, Exp, ?OPTS).

lookup_req() ->
    ?assertMatch(ok, ok).

lookup_by_date_req() ->
    ?assertMatch(ok, ok).
