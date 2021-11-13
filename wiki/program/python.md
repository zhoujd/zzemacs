Python
======

## Build python from source

    $ wget https://www.python.org/ftp/python/2.7.14/Python-2.7.14.tgz
    $ tar xf Python-2.7.14.tgz
    $ cd Python-2.7.14
    $ ./configure --prefix=$HOME/local
    $ make -j 4
    $ make install

## Remove old versions of Python and pip

    $ python2 -m pip uninstall pip
    $ python3 -m pip uninstall pip

    ## 18.04
    $ sudo apt-get remove python-pip
    ## 20.04
    $ sudo apt-get remove python3-pip

## pip2 installation on ubuntu 20.04

    $ wget https://bootstrap.pypa.io/pip/2.7/get-pip.py
    ## To /usr/local/bin
    $ sudo python2 get-pip.py
    ## To ~/.local/bin
    $ python2 get-pip.py

## Python virtualenv

    ##https://python.land/virtual-environments/virtualenv
    $ python3 -m venv <MYVENV>
