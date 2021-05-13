### self common setting
# http://www.cyberciti.biz/tips/bash-aliases-mac-centos-linux-unix.html
# http://souptonuts.sourceforge.net/chirico/index.php
# http://vanmontfort.be/pub/linux/.bashrc

# ls colors setting
export LS_COLORS=$LS_COLORS:'di=01;33:ln=36'

## norm='\[\033[m\]'
## bold='\[\033[1m\]'
## underline='\[\033[4m\]'
## bold='\[\033[5m\]'
## inverted='\[\033[7m\]'
## red='\[\033[0;31m\]'
## lred='\[\033[1;31m\]'
## green='\[\033[0;32m\]'
## lgreen='\[\033[1;32m\]'
## orange='\[\033[0;33m\]'
## yellow='\[\033[1;33m\]'
## blue='\[\033[0;34m\]'
## lblue='\[\033[1;34m\]'
## mangenta='\[\033[1;35m\]'
## white='\[\033[1;37m\]'

color_prompt() {
    local lred='\[\033[1;31m\]'
    local green='\[\033[0;32m\]'
    local title='\[\033]0;\w\007\]'

    if [ $(whoami) = 'root' ]; then
        pmt_str='# '
        pmt=${lred}${pmt_str}${norm}
    else
        pmt_str='$ '
        pmt=${green}${pmt_str}${norm}
    fi

    # depend on term type
    case "$TERM" in
        xterm* | rxvt* )
            PS1="${green}\u@\h \W${norm}${pmt}"
            PS1="${title}${PS1}"
            ;;
        eterm* | emacs* )
            PS1="${green}\u@\h \W${norm}${pmt}"
            ;;
        dumb* )
            PS1="\u@\h \W${pmt_str}"
            ;;
    esac
    export PS1
}

color_prompt
