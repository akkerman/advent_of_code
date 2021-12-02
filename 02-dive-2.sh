#!/usr/bin/env bash

f=0
d=0
a=0

while read line; do
  readarray -d ' ' -t cmd <<<"$line"
  x=${cmd[1]}
  case ${cmd[0]} in
    down)
      a=$(($a + $x))
      ;;
    up)
      a=$(($a - $x))
      ;;
    forward)
      f=$(($f + $x))
      d=$(($d + $a * $x))
      ;;
  esac
done

echo $(($f*$d))
