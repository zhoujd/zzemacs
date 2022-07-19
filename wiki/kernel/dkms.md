DKMS
====

## DKMS commands

    ## DKMS Setup
    $ sudo apt install build-essential dkms

    ## List managed DKMS modules
    $ dkms status

    ## List installed DKMS Debian packages
    $ dpkg -l "*-dkms"

    ## Install
    $ sudo dkms install vboxhost/5.0.10 -k 4.2.0-22-generic

    ## Uninstalling
    $ dkms remove <module name>/<module version> --all

## Build DKMS module from git source

    ## Prepare sources
    $ git clone <some repo> src
    $ cd src
    $ NAME=<module name>
    $ VERSION=<module version>
    $ mkdir /usr/src/${NAME}-${VERSION}
    $ git archive driver-${VERSION} | tar -x -C /usr/src/${NAME}-${VERSION}

    ## And build using DKMS
    $ dkms add     -m ${NAME} -v ${VERSION}
    $ dkms build   -m ${NAME} -v ${VERSION}
    $ dkms install -m ${NAME} -v ${VERSION}

    ## Finally check if “dkms status” lists the module with state “installed” for your kernel version
