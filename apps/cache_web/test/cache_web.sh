#!/usr/bin/env bash

set -eu

declare -A SERVERS=(
    [httpbin]='https://httpbin.org/post'
    [local]='http://localhost:8080/api/cache_server'
)

req() {
    local ENDPOINT=
    local NAME=${1:-}

    if [[ ! -v SERVERS[$NAME] ]]; then
        echo "Unknown server name: $NAME"
        echo "Allowed values are: ${!SERVERS[*]}"
        exit 1
    fi
    ENDPOINT=${SERVERS[$NAME]}

    local BASENAME=
    local ACTION=${2:-}
    case $ACTION in
        i | insert)
            BASENAME='insert'
            ;;
        l | lookup)
            BASENAME='lookup'
            ;;
        d | lookup_by_date)
            BASENAME='lookup_by_date'
            ;;
        *)
            echo "Unknown action: $ACTION"
            echo "Allowed values are"
            echo
            echo "i, insert"
            echo "l, lookup"
            echo "d, lookup_by_date"
            exit 1
            ;;
    esac

    local FILE="${PWD}"/requests/"${BASENAME}".json

    if [[ ! -r $FILE ]]; then
        echo "File is not readable: $FILE"
        exit 1
    fi

    curl -X POST -s -H 'Content-Type: application/json' -d @"${FILE}" "$ENDPOINT"
}

req 'local' "$@"


