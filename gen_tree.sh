#!/usr/bin/env bash

gen_tree () {
    local d=$1
    shift
    local f=$1
    shift
    printf "Generate Tree [%s/%s]:\n" $d $f
    (cd $d;
     ROOT=${1:-main} bap-objdump $f -l ../../sqlite3 -l ../../sqlite3EZ -l ../../callstringtree "$@";
     echo;
     true
    )
}
export -f gen_tree

find testcases -name exe -print0 |
    parallel -0 -k gen_tree {//} {/} "$@"
