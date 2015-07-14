#!/bin/bash

input_num=$1
if [ "$input_num" = "" ];then
    echo "Please input the channel number: ./calc.sh number."
    exit
fi

cpu_mem_file="temp/cpu_mem.txt"
cpu_core_num=`cat /proc/cpuinfo | grep -c processor`
eval $(awk '{a+=$1;b+=$2} END {printf("avgcpu=%.1f\navgmem=%.1f", (a/(NR*'"$cpu_core_num"'))*'"$input_num"',(b/NR));}' $cpu_mem_file)

echo "$avgcpu"
echo "$avgmem"
