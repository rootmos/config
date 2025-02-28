. ~/.profile
. ~/.bash_aliases
. <(dircolors ~/.dircolors)

PS1='\w '
export MANWIDTH=79
export GPG_TTY=$(tty)

export TELEPRESENCE_BINARY=/home/gustav/root/telepresence/bin/telepresence

. /home/gustav/.opam/opam-init/init.sh &> /dev/null || true

if command -v k > /dev/null; then
    . <(k -c)
fi

if command -v zones > /dev/null; then
    . <(zones completion-script)
fi

NPM_PACKAGES="$HOME/.npm-packages"
PATH="$NPM_PACKAGES/bin:$PATH"

function vim() {
    echo 1>&2 "use e!"
}

p() {
    cd "$HOME/git/$1" && tmux rename-window "$1"
}
