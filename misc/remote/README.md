README
======

## Install remote

    $ host=<host>
    $ ssh ${host} "mkdir ~/zach"
    $ scp -r remote ${host}:zach

## Uninstall remote

    $ host=<host>
    $ ssh ${host} "rm -rf ~/zach"
