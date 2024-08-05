### emacs ssh

_complete_ssh_hosts ()
{
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    comp_ssh_hosts=`cat ~/.ssh/config ~/.ssh/config.d/* 2>/dev/null | \
                    grep --color=never "^Host " | \
                    grep -v '[?*]' | \
                    awk '{print $2}'
                    `
    COMPREPLY=( $(compgen -W "${comp_ssh_hosts}" -- ${cur}) )
    return 0
}
complete -F _complete_ssh_hosts ssh
