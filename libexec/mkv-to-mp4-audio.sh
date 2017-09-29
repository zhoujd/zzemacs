#!/bin/sh

## Use "ffmpeg -i file-name" to get map info 

SOURCE="mkv"
TARGET="mp4"
AUDIO_MAP="-map 0:a"

for abc in *.$SOURCE; do
    name=${abc%.*}
    echo "Convert $name.$SOURCE to $name.$TARGET"
    ffmpeg -i "$abc" -vn -acodec copy $AUDIO_MAP "${name}.$TARGET"
done
