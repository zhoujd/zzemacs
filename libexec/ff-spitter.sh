#!/bin/sh

if [ ! $# = 4 ] ; then
    echo "Please use: $0 <input> <start-time> <end-time> <output>"
    exit 0
fi

INPUT_FILE=$1
START_TIME=$2
END_TIME=$3
OUTPUT_FILE=$4

ffmpeg -i $INPUT_FILE -ss $START_TIME -to $END_TIME -vcodec copy -acodec copy $OUTPUT_FILE
