SCP in script
===============

## First Choice

    #!/bin/bash
    IP=192.168.1.179
    PASSWD=123456
    content=$(cat <<!
                spawn scp -r root@$IP:/home/CRM /home
                send "yes\n"
                expect password:
                send "$PASSWD\n"
                expect eof
    )
    echo "$content" | expect

## Second Choice:

    # yum install expect
    # vi scp.exp

          #!/usr/bin/expect -f
          spawn scp -r root@192.168.1.179:/home/CRM /home
          set timeout 10
          expect "root@192.168.1.179's password:"
          exec sleep 1
          send "123456\n"
          interact

    run script

          #expect scp.exp
    or

          #chmod +x scp.exp
          #./scp.exp

## Third Choice

    #!/bin/bash
    IP=192.168.1.179
    PASSWD=123456
    content=$(cat <<!
        spawn scp -r root@$IP:/home/CRM /home
        send "yes\n"
        expect password:
        send "$PASSWD\n"
        expect "Are you sure you want to continue connecting (yes/no)?" { send "yes\r" } "Password:" { send "$PASSWD\r" }
        expect eof
    )
    echo "$content" | expect
