README
======

## Install remote

    $ host=<host>
    $ ssh-copy-id ${host}
    $ ssh ${host} "mkdir -p ~/zach"
    $ scp -r remote ${host}:zach/
    $ ssh ${host} "~/zach/remote/emacs-profile.sh"
    $ ssh ${host} "~/zach/remote/git-config.sh"
    $ ssh ${host} "~/zach/remote/tool.sh"
    $ ssh ${host} "~/zach/remote/install-bin.sh"

## Uninstall remote

    $ host=<host>
    $ ssh ${host} "rm -rf ~/zach"
