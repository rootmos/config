. ~/.profile
. ~/.bash_aliases
. ~/.kubectl_aliases

PS1='\u@\h:\w\$ '
export MANWIDTH=79
export GPG_TTY=$(tty)

export NOTES_DIR=/home/gustav/git/notes

. /home/gustav/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# ssh-agent

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi

if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)"
fi
