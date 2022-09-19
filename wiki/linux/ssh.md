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

## SSH jump host

    ## https://wiki.gentoo.org/wiki/SSH_jump_host
    ## Dynamic jump host list
    ## Use the -J option to jump through a host
    $ ssh -J host1 host2
    $ ssh -J user1@host1:port1 user2@host2 -p port2
    $ ssh -J user1@host1:port1,user2@host2:port2 user3@host3

    ## Static jump host list
    $ cat ~/.ssh/config <<EOF
    ### First jump host. Directly reachable
    Host betajump
      HostName jumphost1.example.org

    ### Host to jump to via jumphost1.example.org
    Host behindbeta
      HostName behindbeta.example.org
      ProxyJump  betajump
    EOF

    $ ssh behindbeta

    ## If usernames on machines differ, specify them by modifing the correspondent ProxyJump line
    $ cat ~/.ssh/config <<EOF
    ProxyJump  otheruser@behindalpha
    EOF

    ## Multiple jumps
    $ cat ~/.ssh/config <<EOF
    ### First jump host. Directly reachable
    Host alphajump
      HostName jumphost1.example.org

    ### Second jumphost. Only reachable via jumphost1.example.org
    Host betajump
      HostName jumphost2.example.org
      ProxyJump alphajump

    ### Host only reachable via alphajump and betajump
    Host behindalphabeta
      HostName behindalphabeta.example.org
      ProxyJump betajump
    EOF
    $ ssh behindalphabeta

## SSH Bastion/Jumphost configuration

    ## https://blog.keyboardinterrupt.com/ansible-jumphost/
    $ ssh -o ProxyCommand='ssh -W %h:%p your_user@bastion' your_user@target
    $ ssh -o ProxyCommand='ssh -W %h:%p jd-desktop' 127.0.0.1
    $ ssh -o ProxyCommand='ssh -W %h:%p -q jd-desktop' 127.0.0.1

    ## Add a bastion host configuration to your ansible inventory
    ansible_ssh_common_args: '-o ProxyCommand="ssh -W %h:%p -q your_user@192.168.1.20"'

## Use sshuttle instead of ssh port fowarding

    $ sudo apt install sshuttle
    $ sshuttle -r your_user@192.168.1.20 10.10.10.0/24
