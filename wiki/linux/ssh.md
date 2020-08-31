ssh
===

1. How to Setup Reverse SSH Tunnel on Linux

        ## run on 172.32.1.1
        $ ssh -fN -R 12345:localhost:22 YOUIP
        $ ssh -R 12345:localhost:22 YOURIP

        $ ssh -p 12345 localhost

2. Reverse ssh tunnel: connection refused

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
