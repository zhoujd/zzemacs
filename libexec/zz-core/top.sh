#/bin/bash

Max_CPU=0
Avg_CPU=0
Total_Time=1

App=$0
EXC_PATH=${App%tools/top.sh}
Interval=$1
rm -f ./cpu_mem.txt
#mkdir -p output

LogFile=./cpu_mem.txt

while sleep $Interval
#while true
do
    #collect cpu and mem usage
    top -d 1 -bn 1 > ./cpu-mem-tmp
    cat ./cpu-mem-tmp | grep StreamingMediaT |grep -v grep|awk '{print $9"\t"$10}'| grep -v 0.0 >> $LogFile
#    echo "info:"$s
#    echo $s >> $LogFile
    #top  -d 1 -bn 1|grep StreamingMediaT |grep -v grep|awk '{print $9"\t"$10}'| grep -v 0.0 >> $LogFile
done
