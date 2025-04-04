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

## Test qsv

    $ dd if=/dev/urandom bs=115200 count=300 of=test.yuv # 10 seconds video
    $ ffmpeg -y -init_hw_device qsv=hw -filter_hw_device hw -f rawvideo -pix_fmt yuv420p -s:v 320x240 -i test.yuv -vf hwupload=extra_hw_frames=64,format=qsv -c:v h264_qsv -b:v 5M test.mp4
    $ ffprobe -v error -show_streams test.mp4
    $ ffmpeg -hwaccel qsv -c:v h264_qsv -i test.mp4 -f null /dev/null

## Get frame type for specific frame using ffmpeg

    $ ffprobe video.mp4 -show_frames | grep -E 'pict_type|coded_picture_number'
    $ ffprobe video.mp4 -show_frames | grep -w -E 'coded_picture_number=8' -B 1

## Finding frame sizes in an encoded bit stream

    ## https://stackoverflow.com/questions/21848514/finding-frame-sizes-in-an-encoded-bit-stream
    ## Display order
    $ ffprobe -show_frames BQMall_832x480_60_QP22.hevc | grep pkt_size

    ## Stored order
    $ ffprobe -show_packets file.hevc | grep size

## Hide banner

    $ ffmpeg -hide_banner
    $ ffprobe -hide_banner


# Print the number of video frames in stream

    ## Calulate frame number
    $ ffprobe -count_frames -v error -select_streams v:0 -show_entries stream=nb_read_frames -of default=nokey=1:noprint_wrappers=1 ${1}
    ## Calulate es bitrate with python
    #!/usr/bin/env python3
    def calculate_bitrate(es_file, frame_num, fps):
        size = os.path.getsize(es_file)
        size_bits = float(size) * 8 * fps / frame_num
        size_kbits = size_bits / 1000
        return int(size_kbits)
    print("{}K".format(calculate_bitrate("test.h264", 270, 30)))
    print("{}K".format(calculate_bitrate("test.h265", 270, 30)))
