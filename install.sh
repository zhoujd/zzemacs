#!/bin/sh

ZZEMACS_ROOT=`pwd`

echo install .emacs to HOME directory begin...

##install package for emacs
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        sudo apt-get install -y emacs
        sudo apt-get install -y cscope
        sudo apt-get install -y texinfo
esac

##setup .emacs
rm -f ~/.emacs
cat > ~/.emacs <<EOF
;;;this is .emacs for zhoujd.
(defvar zzemacs-path "${ZZEMACS_ROOT}/")
(if (file-exists-p (concat zzemacs-path ".emacs"))
    (load-file (concat zzemacs-path ".emacs"))
    (message "zzemacs has not install"))
EOF

##git setting
git config user.name  "zhoujd"
git config user.email "zjd-405@163.com"

##install font
FONT_HOME=/usr/share/fonts/truetype/
sudo cp -f ${ZZEMACS_ROOT}/font/consola.ttf  ${FONT_HOME}
sudo cp -f ${ZZEMACS_ROOT}/font/MSYHMONO.ttf ${FONT_HOME}

##install pymacs
cd ${ZZEMACS_ROOT}/third-party/python
./install.sh
cd ${ZZEMACS_ROOT}

##install pde
cd ${ZZEMACS_ROOT}/site-lisp/pde
perl ./Build.PL
perl ./Build test
perl ./Build
perl ./Build install
cd ${ZZEMACS_ROOT}

##install EPL
cd ${ZZEMACS_ROOT}/third-party/perl/EPL
perl Makefile.PL
make
sudo make install
cd ${ZZEMACS_ROOT}

echo install .emacs to HOME directory end...

