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

app_name=$1
log_file=cpu_mem.txt
interval=1

echo "cpu mem data file: $log_file"
echo "press Ctrl + c for exit"

# clear data
rm -f $log_file

while true ; do
    # collect cpu and mem usage
    top -d 1 -bn 1 -c | grep $app_name | grep -v grep | awk '{print $9"\t"$10}' | grep -v 0.0 >> $log_file

    # sleep for next
    sleep $interval
done
