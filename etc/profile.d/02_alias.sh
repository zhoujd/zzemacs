### self alias setting

# alias setting
alias ls='ls --color=auto'         # ls output with color
alias ll='ls -la'                  # ls for ll
alias l='ls -l'                    # ls for l
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
alias c='clear'
alias r='fc -e -'                  # typing'r' repeats the last command
alias tf='tail -f'
alias et='emacsclient -t'
alias ec='emacsclient -c'
alias ET="SUDO_EDITOR=\"emacsclient -t\" sudo -e"
alias EC="SUDO_EDITOR=\"emacsclient -c\" sudo -e"
alias lcsh="csh -l"

#alias ..='cd ..'
#alias ...='cd ../..'

alias minfo='egrep "Mem|Cache|Swap" /proc/meminfo'
alias top10='sort|uniq -c|sort -n -r|head -n 10'
alias netcheck='nmap -sP $(ip -o addr show | grep inet\ | grep eth | cut -d\  -f 7)'
alias wget='wget -c'
alias userinfo='getent passwd|column  -t -s: -n'
alias groupinfo='getent group|column  -t -s: -n'
alias lsmount='mount|sort|column -t'
alias syslog="tail -F /var/log/syslog"
alias sl="syslog"
alias last='last -a'
alias date='date -R'
alias less='less -R -S -#2'
alias rmr='rm -r -I'
alias cpr='cp -r'
alias scpr='scp -r'
alias date='date -R'
alias colorless='ccze -A|less'
alias cl='colorless'

## more sudo alias
if [ $UID -ne 0 ]; then
    alias reboot='sudo reboot'
    alias poweroff='sudo poweroff'
    alias yum='sudo yum'
    alias apt-get='sudo apt-get'
    alias zypper='sudo zypper'
fi

