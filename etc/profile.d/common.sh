## .bashrc setting for self

# alias setting
alias ls='ls --color=auto'         # ls output with color
alias la='ls -Al'                  # show hide files
alias lx='ls -lXB'                 # sort by extend
alias lk='ls -lSr'                 # sort by size
alias lc='ls -lcr'                 # sort by time
alias lu='ls -lur'                 # sort by access time
alias lr='ls -lR'                  # recurse ls
alias lt='ls -ltr'                 # sort by date
alias lm='ls -al | more'           # pipeline to more
alias tree='tree -Csu'             # another better way for 'ls'
alias which='type -all'
alias ..='cd ..'
alias ...='cd ../..'
alias path='echo -e ${PATH//:/\\n}'
alias du='du -kh'
alias df='df -kTh'
alias h='history'
alias j='jobs -l'
alias et='emacsclient -t'
alias ec='emacsclient -c'
alias ET="SUDO_EDITOR=\"emacsclient -t\" sudo -e"
alias EC="SUDO_EDITOR=\"emacsclient -c\" sudo -e"
alias lcsh="csh -l"

# shell directory bookmark setting
SHELLBM_FILE=~/.sdirs

alias m0='alias g0="cd `pwd`"'
alias m1='alias g1="cd `pwd`"'
alias m2='alias g2="cd `pwd`"'
alias m3='alias g3="cd `pwd`"'
alias m4='alias g4="cd `pwd`"'
alias m5='alias g5="cd `pwd`"'
alias m6='alias g6="cd `pwd`"'
alias m7='alias g7="cd `pwd`"'
alias m8='alias g8="cd `pwd`"'
alias m9='alias g9="cd `pwd`"'

alias lma='alias | grep -e "alias g[0-9]" | grep -v "alias m" | sed "s/alias //"'
alias mdump="alias | grep -e 'alias g[0-9]' | grep -v 'alias m' > $SHELLBM_FILE"
alias mcls="rm -f $SHELLBM_FILE && touch $SHELLBM_FILE"

touch $SHELLBM_FILE
source $SHELLBM_FILE

# ls colors setting
export LS_COLORS='di=01;35:ln=04'
