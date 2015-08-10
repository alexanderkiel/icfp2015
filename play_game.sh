#!/bin/bash

if [ -z $1 ]; then
    echo 'usage: ./play_game.sh -name "Georg3" -f problems/problem_8.json -d 5 [-s idx -submit 1] [-simple 1]\n';
    exit 1;
else
    lein with-profile production run $@
 fi
