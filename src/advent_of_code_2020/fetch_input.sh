#!/usr/bin/env sh

FILENAME="resources/day-$(printf "%02d" "${1}").txt"

curl "https://adventofcode.com/2020/day/${1}/input"\
  -H "Cookie: session=<session>"\
  > $FILENAME
