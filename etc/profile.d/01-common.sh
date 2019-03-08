### self common setting
# http://www.cyberciti.biz/tips/bash-aliases-mac-centos-linux-unix.html
# http://souptonuts.sourceforge.net/chirico/index.php
# http://vanmontfort.be/pub/linux/.bashrc

# ls colors setting
export LS_COLORS='di=01;35:ln=04'

cecho() {
    local exp=$1;
    local color=$2;
    
    if ! [[ $color =~ '^[0-9]$' ]] ; then
        case $(echo $color | tr '[:upper:]' '[:lower:]') in
            black) color=0 ;;
            red) color=1 ;;
            green) color=2 ;;
            yellow) color=3 ;;
            blue) color=4 ;;
            magenta) color=5 ;;
            cyan) color=6 ;;
            white|*) color=7 ;; # white or invalid color
        esac
    fi
    
    tput setaf $color;
    echo $exp;
    tput sgr0;
}

color_prompt() {
    local norm='\[\033[m\]'
    local bold='\[\033[1m\]'
    local underline='\[\033[4m\]'
    local bold='\[\033[5m\]'
    local inverted='\[\033[7m\]'

    local red='\[\033[0;31m\]'
    local lred='\[\033[1;31m\]'
    local green='\[\033[0;32m\]'
    local lgreen='\[\033[1;32m\]'
    local orange='\[\033[0;33m\]'
    local yellow='\[\033[1;33m\]'
    local blue='\[\033[0;34m\]'
    local lblue='\[\033[1;34m\]'
    local mangenta='\[\033[1;35m\]'
    local white='\[\033[1;37m\]'

    # depend on term type
    case "$TERM" in
        xterm* | rxvt* )
            if [ ! "$OS" = "Windows_NT" ]; then
                if [ $(whoami) = 'root' ]; then
                    PS1="${green}[\u@\h \W]${norm}${lred}#${norm} "
                else
                    PS1="${green}[\u@\h \W]${norm}${green}\$${norm} "
                fi

                export PS1="\[\033]0;\w\007\]$PS1"
            fi
            ;;
        eterm* )
            if [ ! "$OS" = "Windows_NT" ]; then
                if [ $(whoami) = 'root' ]; then
                    PS1="${green}[\u@\h \W]${norm}${lred}#${norm} "
                else
                    PS1="${green}[\u@\h \W]${norm}${green}\$${norm} "
                fi
            fi
            ;;
        emacs* )
            if [ "$OS" = "Windows_NT" ]; then
                if [ $(whoami) = 'root' ]; then
                    PS1="${green}[\u@\h \W]${norm}${lred}#${norm} "
                else
                    PS1="${green}[\u@\h \W]${norm}${green}\$${norm} "
                fi
            fi
            ;;
    esac
}

color_prompt
