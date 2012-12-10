#!/bin/sh

echo install .emacs to HOME directory begin...
ZZEMACS_ROOT=`pwd`

##setup .emacs
cat > ~/.emacs <<EOF
;;;this is .emacs for zhoujd.
(defvar zzemacs-path "${ZZEMACS_ROOT}/")
(load-file (concat zzemacs-path ".emacs"))
EOF

##git setting
git config user.name "zhoujd"
git config user.email "zjd-405@163.com"

##install font
FONT_HOME=/usr/share/fonts/truetype/
sudo cp ${ZZEMACS_ROOT}/font/consola.ttf  ${FONT_HOME}
sudo cp ${ZZEMACS_ROOT}/font/MSYHMONO.ttf ${FONT_HOME}

echo install .emacs to HOME directory end...

