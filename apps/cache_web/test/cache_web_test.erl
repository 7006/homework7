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

cache_web_test_() ->
    {
        setup,
        fun setup/0,
        [
            {"Insert", ?_assertStatus(insert, 200)},
            {"Lookup", ?_assertStatus(lookup, 200)},
            {"Lookup By Date", ?_assertStatus(lookup_by_date, 200)}
        ]
    }.

setup() ->
    application:set_env(cache_web, port, 80),
    application:ensure_all_started(cache_web),
    ok.
