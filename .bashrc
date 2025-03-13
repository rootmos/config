. ~/.profile
. ~/.bash_aliases
. <(dircolors ~/.dircolors)

PS1='\w '
export MANWIDTH=79
export GPG_TTY=$(tty)

export TELEPRESENCE_BINARY=/home/gustav/root/telepresence/bin/telepresence

export TEXHELP_REPOSITORY="https://mirror.accum.se/mirror/CTAN/systems/texlive/tlnet/"

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

export HOME_GIT_DIR=$HOME/git
if [ -f "$HOME_GIT_DIR/scripts/p.sh" ]; then
    . "$HOME_GIT_DIR/scripts/p.sh"
fi
