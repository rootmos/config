. "$HOME/.profile"
. "$HOME/.bash_aliases"

LS_COLORS="$(vivid generate ayu)"
export LS_COLORS

PS1='\w '
export MANWIDTH=79
GPG_TTY=$(tty)
export GPG_TTY

#export TEXHELP_MIRROR=https://mirror.accum.se/mirror/CTAN
#export TEXHELP_MIRROR=https://mirror.math.princeton.edu/pub/CTAN

#. /home/gustav/.opam/opam-init/init.sh &> /dev/null || true

if command -v k &>/dev/null; then
    # shellcheck source=/dev/null
    . <(k -c)
fi

if command -v zones &>/dev/null; then
    # shellcheck source=/dev/null
    . <(zones completion-script)
fi

for cmd in transfer-file vpn twitch progress capture-window record-window screenshot; do
    if command -v "$cmd" &>/dev/null; then
        # shellcheck source=/dev/null
        . <("$cmd" --completion-script)
    fi
done

NPM_PACKAGES="$HOME/.npm-packages"
PATH="$NPM_PACKAGES/bin:$PATH"

function vim() {
    echo 1>&2 "use e!"
}

export HOME_GIT_DIR=$HOME/git
SCRIPTS_DIR=$HOME_GIT_DIR/scripts
if [ -d "$SCRIPTS_DIR" ]; then
    . "$SCRIPTS_DIR/render_duration.sh"
    . "$SCRIPTS_DIR/tmp.sh"
    . "$SCRIPTS_DIR/changed.sh"
    . "$SCRIPTS_DIR/p.sh"
    . "$SCRIPTS_DIR/h.sh"
    . "$SCRIPTS_DIR/game.sh"
    . "$SCRIPTS_DIR/K.sh"
    . "$SCRIPTS_DIR/za.sh"

    . "$SCRIPTS_DIR/shlvl.sh"
    PS1="$(shlvl_prefix)$PS1"

    # shellcheck source=/dev/null
    . <("$SCRIPTS_DIR/network" -C)
fi
