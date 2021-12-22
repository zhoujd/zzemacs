ssh
===

## How to Setup Reverse SSH Tunnel on Linux

    ## run on 172.32.1.1
    $ ssh -fN -R 12345:localhost:22 YOUIP
    $ ssh -R 12345:localhost:22 YOURIP

    $ ssh -p 12345 localhost

## Reverse ssh tunnel: connection refused

    ## https://serverfault.com/questions/478171/reverse-ssh-tunnel-connexion-refused
    ## A ==========> B <----X---- C
    ## on B
    $ sudo nano /etc/ssh/sshd_config
    AllowTcpForwarding yes
    GatewayPorts clientspecified

    $ sudo systemctl restart sshd.service

    ## on A
    $ ssh -NR 0.0.0.0:7070:localhost:21 userOnB@B

    ## on C
    $ ssh -p 7070 userOnC@B

## ssh/rsync from one Linux machine to another with a standard ethernet cable

    $ SLAVE_USER_NAME=user
    $ ssh ${SLAVE_USER_NAME}@${SLAVE_IPV6}%${MASTER_INTERFACE}
    $ rsync \
      --recursive \
      --perms \
      --human-readable \
      --progress \
      --verbose \
      -e ssh \
      /path/to/src \
      ${SLAVE_USER_NAME}@[${SLAVE_IPV6}%${MASTER_INTERFACE}]:/path/to/dest

## rsync tool

    ## Important: ALWAYS think about wether you want trailing / chars!
    ## https://lzone.de/cheat-sheet/rsync
    $ rsync -avz  src dest           # content of ./src/ transferred to ./dest/
    $ rsync -avz  src dest/          # content of ./src/ transferred to ./dest/src/
