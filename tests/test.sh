#!/bin/bash
do_test(){
  $1 > result
  a=$(diff result $2 -qs | awk '{ print $NF }')
  if [ $a = "identical" ]; then
    echo "$3 Ok."
  else
    echo "$3 Failed: $(cat result) != $(cat $2)"
  fi
}

do_test "./tab_calc -xi 1 -x 2,3 -c avg -f sample.data" avg_two_columns.res "Avg on two columns"
