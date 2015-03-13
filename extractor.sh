#!/usr/bin/env bash

extract () {
    local d=$1
    shift
    local f=$1
    shift
    printf "Extract [%s/%s]:\n" $d $f
    (cd $d;
     bap-objdump $f -l ../../sqlite3 -l ../../sqlite3EZ -l ../../extractor "$@";
     echo;
     true
    )
}
export -f extract

find testcases -name exe -print0 |
    parallel -0 -k extract {//} {/} "$@"
