### common setting
# http://www.cyberciti.biz/tips/bash-aliases-mac-centos-linux-unix.html
# https://man7.org/linux/man-pages/man5/dir_colors.5.html
# http://souptonuts.sourceforge.net/chirico/index.php
# http://vanmontfort.be/pub/linux/.bashrc

## LS_COLORS in Bash
## run `dircolors -p` for default
# Attribute codes:
# 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
# Text color codes:
# 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
# Background color codes:
# 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white
export LS_COLORS="$LS_COLORS:di=01;34:ln=01;36:mi=01;05;37;41"

## color prompt
# norm='\[\033[m\]'
# bold='\[\033[1m\]'
# underline='\[\033[4m\]'
# bold='\[\033[5m\]'
# inverted='\[\033[7m\]'
# red='\[\033[0;31m\]'
# lred='\[\033[1;31m\]'
# green='\[\033[0;32m\]'
# lgreen='\[\033[1;32m\]'
# orange='\[\033[0;33m\]'
# yellow='\[\033[1;33m\]'
# blue='\[\033[0;34m\]'
# lblue='\[\033[1;34m\]'
# mangenta='\[\033[1;35m\]'
# white='\[\033[1;37m\]'
# gray='\[\033[0;37m\]'
# lgray='\[\033[1;37m\]'
prompt() {
    local green='\[\033[0;32m\]'
    local lgreen='\[\033[1;32m\]'
    local blue='\[\033[0;34m\]'
    local lblue='\[\033[1;34m\]'
    local yellow='\[\033[1;33m\]'
    local norm='\[\033[m\]'
    local title='\[\033]0;\w\007\]'
    case $TERM in
        rxvt* )
            PS1="${lgreen}\u@\h ${lblue}\W${green}\$ ${norm}"
            title='\[\033]0;rxvt:\W $$@\h\007\]'
            PS1="${title}${PS1}"
            ;;
        xterm* )
            PS1="${lgreen}\u@\h ${lblue}\W${green}\$ ${norm}"
            title='\[\033]0;xterm:\W $$@\h\007\]'
            PS1="${title}${PS1}"
            ;;
        dvtm* )
            PS1="${lgreen}\u@\h ${lblue}\W${green}\$ ${norm}"
            ;;
        st* )
            PS1="${lgreen}\u@\h ${lblue}\W${green}\$ ${norm}"
            title='\[\033]0;st:\W $$@\h\007\]'
            PS1="${title}${PS1}"
            ;;
        screen* )
            PS1="${lgreen}[\u@\h \W]\$ ${norm}"
            title='\[\033]0;screen:\W $$@\h\007\]'
            PS1="${title}${PS1}"
            ;;
        tmux* )
            PS1="${green}[\u@\h \W]\$ ${norm}"
            title='\[\033]0;tmux:\W $$@\h\007\]'
            PS1="${title}${PS1}"
            ;;
        eterm* | putty* )
            PS1="${lgreen}\u@\h ${lblue}\W${green}\$ ${norm}"
            ;;
        dumb* | emacs* )
            PS1="\u@\h \W\$ "
            ;;
        linux* )
            export TERM=xterm-256color
            PS1="${lgreen}\u@\h \W${green}\$ ${norm}"
            ;;
    esac
    export PS1
}

## PS1 setting
prompt

## X11-forwarding display settings
if [ -n "$DISPLAY" ]; then
    export LIBGL_ALWAYS_INDIRECT=1
fi
