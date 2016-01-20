
FFMPEG
====================================

0. Download FFMPEG
    http://ffmpeg.org/

    For Windows builds
    http://ffmpeg.zeranoe.com/builds/

1. Merge multiable mp4 in for one
    
    ffmpeg -i 1.mp4 -vcodec copy -acodec copy -vbsf h264_mp4toannexb 1.ts
    ffmpeg -i 2.mp4 -vcodec copy -acodec copy -vbsf h264_mp4toannexb 2.ts
    ffmpeg -i "concat:1.ts|2.ts" -acodec copy -vcodec copy -absf aac_adtstoasc output.mp4

2. Mp4 to h264

    ffmpeg -i input.mp4 -an -vcodec copy -bsf h264_mp4toannexb -f h264 output.h264
