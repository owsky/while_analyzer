#!/bin/sh

contains() {
    local needle="$1"
    while [ $# -gt 1 ]
    do
        shift
        if [ "$needle" = "$1" ]
        then
            return 0
        fi
    done
    return 1
}

executable="$1"
shift
if echo "$executable" | grep -q : >/dev/null
then
    if contains -- "$@"
    then exec cabal v2-run "$executable" -v0 "$@"
    else exec cabal v2-run "$executable" -v0 -- "$@"
    fi
else
    if contains -- "$@"
    then exec cabal v2-run exe:"$executable" -v0 "$@"
    else exec cabal v2-run exe:"$executable" -v0 -- "$@"
    fi
fi
