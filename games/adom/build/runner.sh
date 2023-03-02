#!/bin/sh

GAME=$(readlink -f "$(dirname "$(readlink -f "$0")")/../share/adom")

export LD_LIBRARY_PATH="$GAME/lib64:$LD_LIBRARY_PATH"
cd $GAME
exec ./adom "$@"
