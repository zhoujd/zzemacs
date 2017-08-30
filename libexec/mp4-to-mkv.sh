#!/bin/sh

SOURCE="mp4"
TARGET="mkv"

for abc in *.$SOURCE; do
    name=${abc%.*}
    echo "Convert $name.$SOURCE to $name.$TARGET"
    ffmpeg -i "$abc" -vcodec copy -acodec copy "$name.$TARGET"
done
