#!/bin/bash

# Function to change wallpaper in Xfce for each monitor
change_wallpaper() {
    xfconf-query -c xfce4-desktop -p "/backdrop/screen0/monitor$1/workspace0/last-image" -s "$2"
}

# Function to get the next JPG file in the directory
get_next_wallpaper() {
    local current_wallpaper="$1"
    local next_wallpaper=$(find "$2" -maxdepth 1 -type f -name "*.jpg" | grep -A 1 "$current_wallpaper" | tail -n 1)
    echo "$next_wallpaper"
}

# Main loop
moitor_list=(HDMI-0 DVI-D-0 VGA-0) 
wallpaper_dir="$HOME/Pictures/sunset_wps"
current_wallpaper="$wallpaper_dir/sunset0.jpg"
echo "Wallpapers changed in Xfce."

while true; do
    for monitor in ${monitor_list[@]}; do
        next_wallpaper=$(get_next_wallpaper "$current_wallpaper" "$wallpaper_dir")
        change_wallpaper "$monitor" "$next_wallpaper"
    done
    current_wallpaper="$next_wallpaper"
    sleep 60
done &
