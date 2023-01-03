libreoffice
===========

## Disable logo

    sudo nano /etc/libreoffice/sofficerc
    logo=0

## Window font

    ##Substituting Calibri And Cambria Fonts
    $ sudo apt install fonts-crosextra-carlito fonts-crosextra-caladea

## Install the default Sifr icons:

    $ sudo apt install libreoffice-style-sifr
    Tools->Options->Libreoffice->View->Icon style: Sifr

## Install suits

    $ sudo apt install libreoffice
    $ libreoffice

## Install Database

    $ sudo apt install libreoffice-base

## Install OnlyOffice

    ## https://github.com/ONLYOFFICE/appimage-desktopeditors
    $ wget https://download.onlyoffice.com/install/desktop/editors/linux/DesktopEditors-x86_64.AppImage
    $ chmod a+x DesktopEditors-x86_64.AppImage
    $ ./DesktopEditors-x86_64.AppImage
