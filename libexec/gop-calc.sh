#!/bin/sh

# http://stackoverflow.com/questions/24010875/how-to-calculate-gop-size-of-a-file-h264
# ffprobe -show_frames input.bin | grep key_frame

ffprobe -show_frames $1 > output.txt

GOP=0;

while read p; do
    if [ "$p" = "key_frame=0" ]
    then
        GOP=$((GOP+1))
    fi

    if [ "$p" = "key_frame=1" ]
    then
        echo $GOP
        GOP=0;
    fi
done < output.txt
