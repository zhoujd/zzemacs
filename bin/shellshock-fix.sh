#!/bin/bash

### https://shellshocker.net/#fix

#sudo sh -c 'echo "/usr/local/bin/bash" >> /etc/shells'
#chsh -s /usr/local/bin/bash
#sudo mv /bin/bash /bin/bash-backup
#sudo ln -s /usr/local/bin/bash /bin/bash

##Import vars and functions
. sample.sh

echo "update bash start ..."

## update bash for os
update_bash()
{
    # dectect OS version
    if [ "$OS_DISTRO" = "SUSE" ]; then
        sudo zypper ref -s && zypper up bash 
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        sudo apt-get update
        sudo apt-get install -y bash
    elif [ "$OS_DISTRO" = "CentOS" ]; then
        sudo yum update -y bash
    else
        echo "You are about to install on a non supported linux distribution."
    fi        
}

run_cmd update_bash

echo "update bash end ..."
