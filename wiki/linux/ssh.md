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
    ## Full system backup
    $ rsync -aAXHv --exclude={"/dev/*","/proc/*","/sys/*","/tmp/*","/run/*","/mnt/*","/media/*","/lost+found"} / /path/to/backup

## Generating Ed25519 Key

    ## Generate a new SSH key that uses Ed25519 algorithm
    $ ssh-keygen -o -a 100 -t ed25519 -f ~/.ssh/id_ed25519 -C "zach@example.com"
    ## Adding Your Key to SSH Agent
    $ eval "$(ssh-agent -s)"
    $ ssh-add ~/.ssh/id_ed25519
    $ cat ~/.ssh/config
      Host *
      AddKeysToAgent yes
      UseKeychain yes
      IdentityFile ~/.ssh/id_ed25519
      IdentityFile ~/.ssh/id_rsa # Keep any old key files if you want
    ## Specifying Specific Key to SSH into a Remote Server
    $ ssh -i ~/.ssh/id_ed25519 zach@198.222.111.33
