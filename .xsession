#!/bin/sh

dv
xset m 0 0
xset s off -dpms
hsetroot -solid black

autocutsel -fork &
autocutsel -selection PRIMARY -fork &

(sleep 3 && chromium) &
unclutter &

exec "$HOME/.local/bin/xmonad-x86_64-linux"
