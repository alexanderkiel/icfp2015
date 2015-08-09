#!/usr/bin/env bash

curl -v --user :$API_TOKEN -X POST -H "Content-Type: application/json" \
  -d @solution.json \
  https://davar.icfpcontest.org/teams/305/solutions
