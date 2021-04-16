TMP=$(mktemp -d)
trap 'rm -rf $TMP' EXIT

GIT_ROOT=$HOME/git
INSTALL_ROOT=$HOME/root

fetch() {
    SYMLINK=
    while getopts "s-" OPT; do
        case $OPT in
            s) SYMLINK=1 ;;
            -) break ;;
            ?) return 2 ;;
        esac
    done
    shift $((OPTIND-1))

    URL=$1
    SHA256=$2
    TARGET=$3
    FETCH_CACHE=${FETCH_CACHE-/tmp}
    CACHE=$FETCH_CACHE/$SHA256

    if [ ! -f "$CACHE" ]; then
        UNVERIFIED=$FETCH_CACHE/unverified/$SHA256
        mkdir -p "$(dirname "$UNVERIFIED")"
        wget --progress=dot --output-document="$UNVERIFIED" "$URL"

        SHA256_UNVERIFIED=$(sha256sum "$UNVERIFIED" | cut -f1 -d' ')
        if [ "$SHA256_UNVERIFIED" = "$SHA256" ]; then
            mkdir -p "$(dirname "$CACHE")"
            mv "$UNVERIFIED" "$CACHE"
        else
            echo "sha256 checksum failed ($SHA256_UNVERIFIED != $SHA256): $URL" >&2
            return 1
        fi
    fi

    if [ -n "$SYMLINK" ]; then
        ln -s "$CACHE" "$TARGET"
    else
        cp "$CACHE" "$TARGET"
    fi
}

if [ -n "${APP-}" ]; then
    ROOT=$INSTALL_ROOT/$APP

    if [ -n "${GIT_URL-}" ]; then
        SRC=$GIT_ROOT/$APP

        if [ ! -d "$SRC" ]; then
            git clone --recursive "$GIT_URL" "$SRC"
        fi

        if [ -n "${GIT_REV-}" ]; then
            (cd "$SRC" && git fetch && git checkout "$GIT_REV")
        fi

        if [ -n "${GIT_BRANCH-}" ]; then
            (cd "$SRC" && git fetch && git checkout "origin/$GIT_BRANCH")
        fi
    fi

    if [ -n "${TARBALL_URL-}" ]; then
        TARBALL=$TMP/$(basename "${TARBALL_URL}")
        fetch "$TARBALL_URL" "$TARBALL_SHA256" "$TARBALL"
    fi

    if [ "${TARBALL-}" ]; then
        SRC=$TMP/src
        mkdir "$SRC"
        tar -xvf "$TARBALL" \
            --strip-components="${TARBALL_STRIP_COMPONENTS-1}" \
             -C "$SRC"
    fi
fi

if [ -n "${DEPS-}" ]; then
    echo "installing dependencies: $DEPS"
    sudo -A pacman -S --needed "${DEPS[@]}"
fi

m() {
    make -j"${J-$((2*$(nproc)))}" "$@"
}
