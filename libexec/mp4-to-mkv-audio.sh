#!/bin/sh

## Use "ffmpeg -i file-name" to get map info 

SOURCE="mp4"
TARGET="mkv"
AUDIO_MAP="-map 0:1 -map 0:2"

for abc in *.$SOURCE; do
    name=${abc%.*}
    echo "Convert $name.$SOURCE to $name.$TARGET"
    ffmpeg -i "$abc" -vn -acodec copy $AUDIO_MAP "${name}.$TARGET"
done

