#/bin/bash

# check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

# input parameter check
if [ ! "$#" = "1" ] ; then
    echo "Usage: `basename $0` <app-name>"
    exit 1;
fi

AppName=$1
LogFile=cpu_mem.txt
Interval=1

echo "cpu mem data file: $LogFile"

# clear data
rm -f $LogFile

while true
do
    # collect cpu and mem usage
    top -d 1 -bn 1 -c | grep $AppName | grep -v grep | awk '{print $9"\t"$10}' | grep -v 0.0 >> $LogFile

    # sleep for next
    sleep $Interval
done
