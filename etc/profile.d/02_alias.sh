## self alias setting

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
alias grep='grep --color=auto'

#alias ..='cd ..'
#alias ...='cd ../..'
