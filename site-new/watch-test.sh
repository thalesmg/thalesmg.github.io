#!/bin/bash

set -x

while true; do
  clear
  "$@"
  inotifywait -qre close_write,create,delete,move --exclude '^.git|^./.git/' .
done
