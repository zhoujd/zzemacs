### emacs setting

## vterm
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi

## alias
if [ -n "$INSIDE_EMACS" ]; then
    case $TERM in
        dumb* | emacs* ) ## emacs shell/eshell
            alias me='eme'
            alias nnn='enn'
            alias tig='etig'
            alias vi='st -e vi'
            alias vim='st -e vim'
            ;;
        eterm* | xterm* )  ## term and vterm
            alias me='eme'
            ;;
    esac
fi

## emacsclient
alias ET="SUDO_EDITOR=\"emacsclient -t\" sudo -e"
alias EC="SUDO_EDITOR=\"emacsclient -c\" sudo -e"
