. ~/.profile
. ~/.bash_aliases
. <(dircolors ~/.dircolors)

PS1='\w '
export MANWIDTH=79
export GPG_TTY=$(tty)

export TEXHELP_REPOSITORY="https://mirror.accum.se/mirror/CTAN/systems/texlive/tlnet/"

. /home/gustav/.opam/opam-init/init.sh &> /dev/null || true

if command -v k > /dev/null; then
    . <(k -c)
fi

if command -v zones > /dev/null; then
    . <(zones completion-script)
fi

if command -v transfer-file > /dev/null; then
    . <(transfer-file --completion-script)
fi

NPM_PACKAGES="$HOME/.npm-packages"
PATH="$NPM_PACKAGES/bin:$PATH"

function vim() {
    echo 1>&2 "use e!"
}

export HOME_GIT_DIR=$HOME/git
if [ -d "$HOME_GIT_DIR/scripts" ]; then
    . "$HOME_GIT_DIR/scripts/p.sh"
    . "$HOME_GIT_DIR/scripts/h.sh"
    . "$HOME_GIT_DIR/scripts/game.sh"
    . "$HOME_GIT_DIR/scripts/render_duration.sh"
    . "$HOME_GIT_DIR/scripts/K.sh"

    . "$HOME_GIT_DIR/scripts/shlvl.sh"
    PS1="$(shlvl_prefix)$PS1"
fi
