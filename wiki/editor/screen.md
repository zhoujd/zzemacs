screen
======

## Man screen

```
## https://linux.die.net/man/1/screen
```

## More hardstatus examples

```
## alwaysfirstline/ignore/alwayslastline
hardstatus string "%{.kw}screen %w | %h%? %= %H"
hardstatus string "%{.bW}%-w%{.rW}%n %t%{-}%+w %=%{..G} %{..Y} %H"
hardstatus string "%{.g}[screen %n%?: %t%?] %h"
hardstatus string "%{.g}%w %="
```

## Screen Command: Set Baud Rate

```
## https://www.cyberciti.biz/faq/unix-linux-apple-osx-bsd-screen-set-baud-rate/
screen /dev/ttySX baud_rate,cs8|cs7,ixon|-ixon,ixoff|-ixoff,istrip|-istrip
## OR ##
screen /dev/{console_port} 115200

## Where,
/dev/ttySX: Linux serial port (e.g., /dev/ttyS0 [COM1] )
baud_rate: Usually 300, 1200, 9600 (default), 19200, or 115200. This affects transmission as well as receive speed.
cs8 or cs7: Specify the transmission of eight (or seven) bits per byte.
ixon or -ixon: Enables (or disables) software flow-control (CTRL-S/CTRL-Q) for sending data.
ixoff or -ixoff: Enables (or disables) software flow-control for receiving data.
istrip or -istrip: Clear (or keep) the eight bit in each received byte.
```

## Kill a screen session

```
local PIDs=( $(screen -list | grep screen- | cut -f1 -d'.' | sed 's/\W//g') )
if [ -n "${PIDs[@]}" ]; then
    for pid in "${PIDs[@]}"; do
        sudo screen -XS $pid quit
        sudo kill -9 $pid
    done
    sudo screen -wipe
fi
```
