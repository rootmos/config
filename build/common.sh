GIT_ROOT=$HOME/git
INSTALL_ROOT=$HOME/root

if [ -n "${APP-}" ]; then
    ROOT=$INSTALL_ROOT/$APP

    if [ -n "${GIT_URL-}" ]; then
        SRC=$GIT_ROOT/$APP

        if [ ! -d "$SRC" ]; then
            git clone "$GIT_URL" "$SRC"
        fi

        if [ -n "${GIT_REV-}" ]; then
            (cd "$SRC" && git checkout "$GIT_REV")
        fi
    fi
fi

m() {
    make -j"${J-$((2*$(nproc)))}" "$@"
}
