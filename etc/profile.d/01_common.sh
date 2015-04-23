### self common setting
# http://www.cyberciti.biz/tips/bash-aliases-mac-centos-linux-unix.html
# http://souptonuts.sourceforge.net/chirico/index.php
# http://vanmontfort.be/pub/linux/.bashrc

# ls colors setting
export LS_COLORS='di=01;35:ln=04'

norm='\[\033[m\]'
bold='\[\033[1m\]'
underline='\[\033[4m\]'
bold='\[\033[5m\]'
inverted='\[\033[7m\]'

red='\[\033[0;31m\]'
lred='\[\033[1;31m\]'
green='\[\033[0;32m\]'
lgreen='\[\033[1;32m\]'
orange='\[\033[0;33m\]'
yellow='\[\033[1;33m\]'
blue='\[\033[0;34m\]'
lblue='\[\033[1;34m\]'
mangenta='\[\033[1;35m\]'
white='\[\033[1;37m\]'

if [ $(whoami) = 'root' ]; then
    PS1="${yellow}\u@\h${white}:${norm}\w${norm}${lred}#${norm} "
else
    PS1="${yellow}\u@\h${white}:${norm}\w${norm}\$ "
fi
