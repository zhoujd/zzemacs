#!/bin/sh

ZZEMACS_ROOT=`pwd`

##Import vars and functions
. $ZZEMACS_ROOT/bin/sample.sh

echo "install .emacs to HOME directory begin..."

##setup .emacs
Install_dot_emacs()
{
cat > ~/.emacs <<EOF
;;;This is .emacs for zhoujd.
(defvar zzemacs-path "${ZZEMACS_ROOT}")
(if (file-exists-p (concat zzemacs-path "/.emacs"))
    (load-file (concat zzemacs-path "/.emacs"))
    (message "zzemacs has not install"))
EOF
}

##setup font setting
Install_fonts_conf()
{
    mkdir -p ~/.fonts
    ln -sf ${ZZEMACS_ROOT}/font/* ~/.fonts
}

Install_emacs_run()
{
LANCHER=~/runemacs

cat > ${LANCHER} <<EOF
#!/bin/sh

emacs --no-site-file -q \\
      --eval "(setq zzemacs-path \"${ZZEMACS_ROOT}\")" \\
      --eval "(load-file \"${ZZEMACS_ROOT}/.emacs\")"  \\
      --eval "(message \"start emacs finished.\")"     \\
      $* &
EOF
chmod +x ${LANCHER}
}

Install_other()
{
    ##create ~/.emacs.d folder
    mkdir -p ~/.emacs.d
}

##Install thirdparty
Install_thirdparty()
{
    ##install pymacs
    cd ${ZZEMACS_ROOT}/third-party/python
    ./install.sh
    cd ${ZZEMACS_ROOT}

    ##install pde
    cd ${ZZEMACS_ROOT}/site-lisp/pde
    perl ./Build.PL
    perl ./Build test
    perl ./Build
    sudo perl ./Build install
    cd ${ZZEMACS_ROOT}

    ##install EPL
    cd ${ZZEMACS_ROOT}/third-party/perl/EPL
    perl Makefile.PL
    make
    sudo make install
    cd ${ZZEMACS_ROOT}
}

main()
{
    ##install configure file
    if [ -f ~/.emacs ] ; then
        confirm_execute "Do you wanna overwrite .emacs? [y/N]" try_command Install_dot_emacs
    else
        echo "install configure file"
        try_command Install_dot_emacs
    fi

    echo "install fonts"
    try_command Install_fonts_conf

    echo "install runemacs to $HOME"
    try_command Install_emacs_run

    echo "install others"
    try_command Install_other

    ##install third-party
    confirm_execute "Do you wanna install third-party packages? (y/N): " try_command Install_thirdparty

}

main

echo "install .emacs to HOME directory end..."
