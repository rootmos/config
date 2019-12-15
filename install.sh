#!/bin/bash

set -o nounset -o pipefail -o errexit

ROOT=${ROOT-$(readlink -f "$0" | xargs dirname)}
ACTION=install
DRY=0
while getopts "bhiun" OPT; do
    case $OPT in
        b) PREFIX=$HOME/bin; STRIP=1 ;;
        h) PREFIX=$HOME; STRIP=0 ;;
        n) DRY=1 ;;
        i) ACTION=install ;;
        u) ACTION=uninstall ;;
        ?) exit 2 ;;
    esac
done
shift $((OPTIND-1))

if [ -z "${PREFIX-}" ]; then
    echo "PREFIX not set" 1>&2
    exit 1
fi

SRC=$(readlink -f "$1")
if [ "${STRIP-1}" -eq 1 ]; then
    TARGET=$PREFIX/$(basename "$SRC")
else
    TARGET=$PREFIX/$(sed 's,'"$ROOT"'/,,' <<< "$SRC")
fi

if [ "$ACTION" = "install" ]; then
    echo "slink: $TARGET -> $SRC"
    if [ "$DRY" -eq 0 ]; then
        mkdir -p "$(dirname "$TARGET")"
        ln -s "$SRC" "$TARGET"
    fi
elif [ "$ACTION" = "uninstall" ]; then
    echo "rm: $TARGET"
    if [ "$DRY" -eq 0 ]; then
        rm -rf "$TARGET"
    fi
fi
