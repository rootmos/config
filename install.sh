#!/bin/bash

set -o nounset -o pipefail -o errexit

ROOT=${ROOT-$(readlink -f "$0" | xargs dirname)}
ACTION=install
DRY=0
SUDO=${SUDO-}
while getopts "bhiunrs" OPT; do
    case $OPT in
        b) PREFIX=$HOME/bin; STRIP=1 ;;
        h) PREFIX=$HOME; STRIP=0 ;;
        r) PREFIX=; STRIP=0 ;;
        n) DRY=1 ;;
        i) ACTION=install ;;
        u) ACTION=uninstall ;;
        s) SUDO=sudo ;;
        ?) exit 2 ;;
    esac
done
shift $((OPTIND-1))

if [[ ! -v PREFIX ]]; then
    echo "PREFIX not set" 1>&2
    exit 1
fi

for t in "$@"; do
    SRC=$(readlink -f "$t")
    if [ "${STRIP-1}" -eq 1 ]; then
        TARGET=$PREFIX/$(basename "$SRC")
    else
        TARGET=$PREFIX/$(sed 's,'"$ROOT"'/,,' <<< "$SRC")
    fi

    if [ "$ACTION" = "install" ]; then
        if [ "$DRY" -eq 0 ]; then
            $SUDO mkdir -p "$(dirname "$TARGET")"
            if [ -n "$SUDO" ]; then
                echo "cp: $SRC -> $TARGET"
                $SUDO cp "$SRC" "$TARGET"
            else
                echo "slink: $SRC -> $TARGET"
                ln -fs "$SRC" "$TARGET"
            fi
        fi
    elif [ "$ACTION" = "uninstall" ]; then
        echo "rm: $TARGET"
        if [ "$DRY" -eq 0 ]; then
            $SUDO rm -rf "$TARGET"
        fi
    fi
done
