FFMPEG
====================================

## Download FFMPEG
    http://ffmpeg.org/

    For Windows builds
    http://ffmpeg.zeranoe.com/builds/

    For build ffmpeg via source on Ubuntu 18.04

    $ sudo apt install yasm

    $ ./configure --prefix=/usr ./configure --prefix=/usr --enable-shared
    $ make -j4
    $ sudo make install

## Merge multiable mp4 in for one

    $ ffmpeg -i 1.mp4 -vcodec copy -acodec copy -vbsf h264_mp4toannexb 1.ts
    $ ffmpeg -i 2.mp4 -vcodec copy -acodec copy -vbsf h264_mp4toannexb 2.ts
    $ ffmpeg -i "concat:1.ts|2.ts" -acodec copy -vcodec copy -absf aac_adtstoasc output.mp4

    $ ffmpeg -acodec ac3 -i input.mkv -acodec aac -strict experimental -vcodec copy -map 0:0 -map 0:1 -map 0:2 -map 0:3 -scodec copy output.mkv

## Mp4 to h264/h265

    $ ffmpeg -i input.mp4 -an -vcodec copy -bsf h264_mp4toannexb -f h264 output.h264
    $ ffmpeg -i input.mp4 -an -vcodec copy -bsf hevc_mp4toannexb -f hevc output.h265

## FFMPEG in script

    for %%i in (*.mkv) do ffmpeg.exe -i "%%i" -vcodec copy -acodec copy "%%~ni.mp4"

    for abc in *.mp4; do
        name=${abc%.*}
        echo "$name"
        ffmpeg -i "$abc" -ac 2 -f wav - | lame -V 0 - "$name.mp3"
    done

## Split and merge

    $ ffmpeg -i input.mp4 -ss **START_TIME** -to **STOP_TIME** -acodec copy -vcodec copy output.mp4
    $ ffmpeg -i input.mp4 -ss **START_TIME** -t **DURING_TIME** -acodec copy -vcodec copy output.mp4

    $ cat list.txt
    file '/path/to/file1'
    file '/path/to/file2'
    file '/path/to/file3'
    $ ffmpeg -f concat -i **list.txt** -c copy output.mp4

## FFmpeg build 32 bit execute

    $ sudo apt-get install gcc-multilib g++-multilib module-assistant
    $ ./configure --cc='gcc -m32'

    $ sudo apt install ccache
    $ ./configure --cc='ccache gcc -m32'

## Build ffmpeg with x264 and x265

    $ sudo apt install libx264-dev libx265-dev
    $ ./configure --enable-libx264 --enable-libx265

## Thumbnailer

    $ sudo apt install ffmpegthumbnailer

## Recorde video

    ## slop -f "%x %y %w %h"
    $ sudo apt install slop
    $ ffmpeg -f x11grab -s 1920x1024 -i :0.0+0,40 -c:v libx264 -preset ultrafast -crf 0 /tmp/emacs.mkv
    $ ffmpeg -i /tmp/emacs.mkv /tmp/emacs.mp4 #or whatever output you want - mp4 is about 1/100th the size
