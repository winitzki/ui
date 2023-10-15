#!/bin/bash

function condense() {
  grep -v  '^;' "$1" | \
    grep -v '^\s*$'|sed -e 's|^\([^ ]*\)  *= |\1 =\n    |' | \
    sed -e 's/^\([^ ]*\) =/)\ndef \1[$: P] = P(/; s! ;! // !; s|\([A-Za-z0-9]\)-\([A-Za-z0-9]\)|\1_\2|g;' \
      > "$2"
}

condense dhall/src/main/resources/dhall.abnf.txt dhall/src/main/resources/dhall.condensed.txt

condense dhall/src/main/resources/abnf.txt dhall/src/main/resources/abnf.condensed.txt
