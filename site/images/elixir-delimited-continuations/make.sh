#!/bin/bash

CURDIR=$(dirname $(realpath "$0"))
POST_SLUG=${CURDIR##*/}

for graph in $(ls "$CURDIR"/*.dot)
do
  name=${graph##*/}
  dest=$(realpath "$CURDIR/../$POST_SLUG-$name")
  out=${dest%%.dot}.png
  dot -Tpng "$graph" -o "$out"
done
