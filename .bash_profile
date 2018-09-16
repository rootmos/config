. ~/.profile
. ~/.bash_aliases
. ~/.bash_utils

PS1='\h:\w\$ '
export MANWIDTH=79
export GPG_TTY=$(tty)

export NOTES_DIR=/home/gustav/git/notes
export AUDIO_JOURNAL_SECONDARY=/home/gustav/mnt/raspberry/audio-journal

. /home/gustav/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# ssh-agent

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi

if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)"
fi
