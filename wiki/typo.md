Linux type practice
===================

1. speedpad on Arch Linux
        
    $ sudo pacman -S fortune-mod
    $ git clone https://github.com/feurix/speedpad.git
    $ cd speedpad 
    $ install -m 755 bin/speedpad /usr/local/bin
    $ install -m 644 man/speedpad.1 /usr/local/share/man/man1

2. speedpad on Ubuntu Linux

    $ sudo apt install fortune-mod
    $ sudo apt install speedpad

3. cowsay on Linux

    $ sudo apt install cowsay
    $ sudo pacman -S cowsay
