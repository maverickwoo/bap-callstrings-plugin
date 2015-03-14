#!/usr/bin/env bash

extract () {
    local d=$1
    shift
    local f=$1
    shift
    printf "!!!Extract [%s/%s]:\n" $d $f
    (cd $d;
     (time bap-objdump $f -l ../../sqlite3 -l ../../sqlite3EZ -l ../../extractor "$@") 2>&1;
     echo;
     true
    )
}
export -f extract

find -L testcases -name exe -print0 |
    parallel -0 -k extract {//} {/} "$@"
