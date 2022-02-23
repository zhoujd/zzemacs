# we pass 2 parameters to __git_ps1 as pre and post which are strings.
export PROMPT_COMMAND='__git_ps1 "\u@\h:\W" "\\\$ "'

# if .git-prompt.sh exists, set options and execute it
if [ -f ~/.git-prompt.sh ]; then
    GIT_PS1_SHOWDIRTYSTATE=true
    GIT_PS1_SHOWSTASHSTATE=true
    GIT_PS1_SHOWUNTRACKEDFILES=true
    GIT_PS1_SHOWUPSTREAM="auto"
    GIT_PS1_HIDE_IF_PWD_IGNORED=true
    GIT_PS1_SHOWCOLORHINTS=true
    . ~/.git-prompt.sh
fi
