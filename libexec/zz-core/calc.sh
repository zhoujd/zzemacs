#!/bin/bash

input_num=1
if [ ! $1 = "" ]; then
    input_num=$2 
fi

cpu_mem_file=cpu_mem.txt


cpu_core_num=`cat /proc/cpuinfo | grep -c processor`
eval $(awk '{a+=$1;b+=$2} END {printf("avgcpu=%.1f\navgmem=%.1f", (a/(NR*'"$cpu_core_num"'))*'"$input_num"',(b/NR));}' $cpu_mem_file)

echo "$avgcpu"
echo "$avgmem"
