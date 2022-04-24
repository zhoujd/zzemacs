nnn
===

## Install nnn

    $ git clone https://github.com/jarun/nnn
    $ cd nnn
    $ make
    $ sudo make install

## Setup nnn enviroment variables

    $ sudo tee /etc/profile.d/zz-nnn.sh <<EOF
    export NNN_BMS='d:~/Downloads/;w:~/work/;/:/;z:/zach/'
    export NNN_SSHFS='sshfs -o follow_symlinks'
    export NNN_TRASH=1
    export NNN_FIFO='/tmp/nnn.fifo'
    EOF
