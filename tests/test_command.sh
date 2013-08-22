#!/bin/bash
test_com(){
  echo "tab_calc -xi 1 -x 3,4 -i 4 -f sample.data -c $1"
  tab_calc -xi 1 -x 3,4 -i 4 -f sample.data -c $1
  echo "tab_calc -xi 1 -x 3,4 -i 4 -f sample.data -c $1 -s"
  tab_calc -xi 1 -x 3,4 -i 4 -f sample.data -c $1 -s
}

for com in none avg sum med med_q; do
  test_com ${com}
done