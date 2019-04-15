WIFI
====


1.  wifi scan

        $ sudo iwlist wlan0 scan | awk -F ':' '/ESSID:/ {print $2;}'
        $ sudo iwlist wlan0 scan | perl -nle '/ESSID:(.*)$/ && print $1'

