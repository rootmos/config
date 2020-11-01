. ~/.profile
. ~/.bash_aliases

PS1='\w '
export MANWIDTH=79
export GPG_TTY=$(tty)

export TELEPRESENCE_BINARY=/home/gustav/root/telepresence/bin/telepresence

. /home/gustav/.opam/opam-init/init.sh &> /dev/null || true

if command -v k > /dev/null; then
    . <(k -c)
fi

NPM_PACKAGES="$HOME/.npm-packages"
PATH="$NPM_PACKAGES/bin:$PATH"
