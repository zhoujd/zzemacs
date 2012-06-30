#/bin/sh

echo install .emacs to HOME directory begin...
ZZEMACS_ROOT=`pwd`
rm -f ~/.emacs

echo ";;;this is .emacs for zhoujd.">> ~/.emacs
echo "(defvar zzemacs-path \"${ZZEMACS_ROOT}/\")" >> ~/.emacs
echo "(load-file (concat zzemacs-path \".emacs\"))" >> ~/.emacs

echo install .emacs to HOME directory end...
