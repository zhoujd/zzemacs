#!/bin/bash

## How to update bash
#sudo sh -c 'echo "/usr/local/bin/bash" >> /etc/shells'
#chsh -s /usr/local/bin/bash
#sudo mv /bin/bash /bin/bash-backup
#sudo ln -s /usr/local/bin/bash /bin/bash

cd ~/
mkdir bash
cd bash
wget http://ftp.gnu.org/gnu/bash/bash-4.3.tar.gz
#download all patches
while [ true ]; do i=`expr $i + 1`; wget -N http://ftp.gnu.org/gnu/bash/bash-4.3-patches/bash43-$(printf '%03g' $i); if [ $? -ne 0 ]; then break; fi; done
tar zxvf bash-4.3.tar.gz
cd bash-4.3
for p in `ls ../bash43-[0-9][0-9][0-9]`; do patch -p0 < $p; done

./configure
make
sudo make install
