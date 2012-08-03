#!/bin/sh

echo install .emacs to HOME directory begin...
ZZEMACS_ROOT=`pwd`

##setup .emacs
rm -f ~/.emacs
echo ";;;this is .emacs for zhoujd.">> ~/.emacs
echo "(defvar zzemacs-path \"${ZZEMACS_ROOT}/\")" >> ~/.emacs
echo "(load-file (concat zzemacs-path \".emacs\"))" >> ~/.emacs

##install font
FONT_HOME=/usr/share/fonts/truetype/
sudo cp ${ZZEMACS_ROOT}/font/consola.ttf  ${FONT_HOME}
sudo cp ${ZZEMACS_ROOT}/font/MSYHMONO.ttf ${FONT_HOME}

echo install .emacs to HOME directory end...
