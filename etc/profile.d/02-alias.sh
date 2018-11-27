### self alias setting

## reload .bashrc
alias rl='. ~/.bashrc'

## ls alias
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
alias lh='ls -lh'                  # ls for -h
alias lm='ls -al | more'           # pipeline to more
alias sl='ls'                      # typo

## buildin alias
alias dir='dir --color=auto'
alias dirtree="ls -R | grep :*/ | grep ':$' | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
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
alias top10='sort | uniq -c | sort -n -r | head -n 10'
alias last='last -a'
alias less='less -R -S -#2'
alias rmr='rm -r -I'
alias cpr='cp -r'
alias usage='du -h --max-depth=1'
alias sudo='sudo '

## date alias
alias date='date -R'
alias now='date +"%T"'
alias nowtime='now'
alias nowdate='date +"%d-%m-%Y"'

## others shell alias
alias lcsh="csh -l"

## system alias
alias minfo='egrep "Mem|Cache|Swap" /proc/meminfo'
alias userinfo='getent passwd | column -t -s:'
alias groupinfo='getent group | column -t -s:'
alias lsmount='mount | sort | column -t'
alias syslog='tail -F /var/log/syslog'
alias screenoff='xset dpms force off'

## color show alias
alias colorless='ccze -A | less'   # https://github.com/cornet/ccze.git
alias cl='colorless'

## net alias
alias ports='netstat -tulanp'
alias netcheck='nmap -sP $(ip -o addr show | grep inet\ | grep en | cut -d\  -f 7)'
alias scpr='scp -r'
alias wget='wget -c'
alias ipt='sudo /sbin/iptables'
alias iptlist='sudo /sbin/iptables -L -n -v --line-numbers'
alias iptlistin='sudo /sbin/iptables -L INPUT -n -v --line-numbers'
alias iptlistout='sudo /sbin/iptables -L OUTPUT -n -v --line-numbers'
alias iptlistfw='sudo /sbin/iptables -L FORWORD -n -v --line-numbers'
alias firewall='iptlist'

## edit alias
alias et='emacsclient -t'
alias ec='emacsclient -c'
alias ET="SUDO_EDITOR=\"emacsclient -t\" sudo -e"
alias EC="SUDO_EDITOR=\"emacsclient -c\" sudo -e"

## vlc alias
alias vlc265='vlc --demux hevc'
alias vlc264='vlc --demux h264'

## tmux
alias tmux='TERM=xterm-256color tmux'
alias tnews='tmux new-session -s'
alias tls='tmux list-session'
alias tlw='tmux list-window'
alias tsw='tmux switch -t'
alias tlc='tmux list-command'
alias tat='tmux attach -t'
alias trs='tmux rename-session -t'
alias tks='tmux kill-session -t'

##byobu
alias bnews='byobu new-session -s'
alias bls='byobu list-session'
alias blw='byobu list-window'
alias bsw='byobu switch -t'
alias blc='byobu list-command'
alias bat='byobu attach -t'
alias brs='byobu rename-session -t'
alias bks='byobu kill-session -t'

## nano
alias nano='nano -w'
