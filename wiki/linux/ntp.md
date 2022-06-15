ntp
===

## How to Set or Change the Time Zone in Linux

    $ timedatectl
    $ ls -l /etc/localtime
    $ timedatectl list-timezones
    $ sudo timedatectl set-timezone <your_time_zone>
    $ sudo timedatectl set-timezone Asia/Shanghai
    $ timedatectl

## Changing the Time Zone by Creating a Symlink #

    $ sudo rm -rf /etc/localtime
    $ sudo ln -s /usr/share/zoneinfo/America/New_York /etc/localtime
    $ date

## Install and configure NTP Server on the host computer

    ## https://vitux.com/how-to-install-ntp-server-and-client-on-ubuntu/
    $ sudo apt install ntp
    $ sntp --version
    $ sudo nano /etc/ntp.conf
    $ sudo service ntp restart
    $ sudo service ntp status
    $ sudo ufw allow from any to any port 123 proto udp

## Configure NTP Client to be Time Synced with the NTP Server

    ## https://vitux.com/how-to-install-ntp-server-and-client-on-ubuntu/
    $ sudo nano /etc/hosts
    192.168.100.6 NTP-server-host
    ## Disable the systemd timesyncd service on the
    $ sudo timedatectl set-ntp off
    $ sudo apt install ntp
    $ sudo nano /etc/ntp.conf
    server NTP-server-host prefer iburst
    $ sudo service ntp restart
    $ ntpq -ps

## Use systemd timesyncd service (Prefer)

    ## Error: Unit systemd-timesyncd.service is masked
    $ sudo apt install -y systemd-timesyncd
    $ sudo sed -i 's/^NTP/#NTP/' /etc/systemd/timesyncd.conf
    $ sudo sed -i 's/^FallbackNTP/#FallbackNTP/' /etc/systemd/timesyncd.conf
    $ sudo tee -a /etc/systemd/timesyncd.conf <<EOF
    NTP=ntp.ubuntu.com
    FallbackNTP=ntp.ubuntu.com
    EOF
    $ sudo systemctl enable systemd-timesyncd.service
    $ sudo systemctl restart systemd-timesyncd.service
    $ timedatectl set-timezone Asia/Shanghai
    $ sudo timedatectl --adjust-system-clock
    $ sudo hwclock --systohc --utc
