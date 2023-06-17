#!/bin/bash

echo "please replug in the headset if it connecting"
echo "audio reset start"

pulseaudio -k
sudo alsa force-reload
pulseaudio --start

echo "audio reset end"
