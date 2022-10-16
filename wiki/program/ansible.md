Ansible
=======

## Use a SSH Jump Host With Ansible

    ## https://blog.ruanbekker.com/blog/2020/10/26/use-a-ssh-jump-host-with-ansible/
    ## https://blog.3sky.dev/article/ansible-with-jumphost/
    ## Access monster-3sky-dev only from test-3sky-dev
    $ cat ~/.ssh/config <<EOF
    Host test-3sky-dev
    # test, Suse
    HostName 3.22.11.33
    User kuba
    Port 31
    IdentityFile ~/.ssh/id_rsa_xy

    Host monster-3sky-dev
        # prod, ubuntu
       HostName 12.22.11.33
       User kuba
       Port 45
       ProxyJump test-3sky-dev
    EOF

    ## avoid password authentication
    # on test-3sky-dev machine
    $ ssh-keygen -t rsa -b 4096 -C "test-3sky-dev"
    $ ssh-copy-id -i ~/.ssh/id_rsa_yx.pub kuba@12.22.11.33 -p 45

    ## configure `~/.ssh/config` on my jump station
    Host monster-3sky-dev
       HostName 12.22.11.33
       User kuba
       Port 45
       IdentityFile ~/.ssh/id_rsa_yx

    $ ssh monster-3sky-dev

    $ cat > host.ini <<EOF
    [gpu]
    monster-3sky-dev ansible_python_interpreter=/usr/bin/python3
    EOF

    ##
    $ ANSIBLE_SSH_ARGS="-F /home/kuba/.ssh/config" ansible monster-3sky-dev -m ping -i host.ini

    $ cat > playbook.yml <<EOF
    - name: Test Ping
      hosts: deployment
      tasks:
      - action: ping
    EOF

    $ ANSIBLE_SSH_ARGS="-F /home/kuba/.ssh/config" ansible-playbook playbook.yml -i host.ini

## Install ansible modules

    $ ansible-galaxy collection install ansible.posix
    $ ansible-galaxy collection install community.general

## Execute command on the Ansible host - Ansible localhost

    $ cat > localhost.yml <<EOF
    ---
    - name: localhost demo
      hosts: localhost
      vars:
        ansible_connection: local
        ansible_python_interpreter: "{{ ansible_playbook_python }}"
      tasks:
        - name: print hostname
          ansible.builtin.debug:
            msg: "{{ inventory_hostname }}"
    EOF

    $ ansible-playbook localhost.yml
