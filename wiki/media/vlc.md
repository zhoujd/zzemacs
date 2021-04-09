VLC Something
=============

1. Bat file for VLC video wall

    cd "c:\Program Files (x86)\VideoLAN\VLC"

    timeout /t 60
    start vlc rtsp://ip1/1.ts --no-video-deco --no-embedded-video --video-x=0 --video-y=0 --qt-start-minimized --height=1080 --width=1920 -L
    timeout /t 5 /NOBREAK
    start vlc rtsp://ip2/2.ts --no-video-deco --no-embedded-video --video-x=1919 --video-y=1 --qt-start-minimized --height=360 --width=640 -L
    timeout /t 5 /NOBREAK
