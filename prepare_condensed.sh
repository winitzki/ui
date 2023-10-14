grep -v  '^;' dhall/src/main/resources/dhall.abnf.txt | \
  grep -v '^\s*$'|sed -e 's|^\([^ ]*\)  *= |\1 =\n    |' | \
  sed -e 's/^\([^ ]*\) =/)\ndef \1[$: P] = P(/; s! ; ! // !; s|\([A-Za-z0-9]\)-\([A-Za-z0-9]\)|\1_\2|g;' \
   > dhall/src/main/resources/dhall.condensed.txt
