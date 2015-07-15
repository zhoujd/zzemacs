### self common setting
# http://www.cyberciti.biz/tips/bash-aliases-mac-centos-linux-unix.html
# http://souptonuts.sourceforge.net/chirico/index.php
# http://vanmontfort.be/pub/linux/.bashrc

# ls colors setting
export LS_COLORS='di=01;35:ln=04'

color_prompt()
{
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
        xterm* | rxvt*)
            if [ $(whoami) = 'root' ]; then
                PS1="${yellow}\u@\h${white}:${norm}\w${norm}${lred}#${norm} "
            else
                PS1="${yellow}\u@\h${white}:${norm}\w${norm}\$ "
            fi
            ;;
    esac
}

color_prompt
