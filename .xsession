#!/bin/sh

dv
xset m 0 0
xset s off -dpms
hsetroot -solid black

autocutsel -fork &
autocutsel -selection PRIMARY -fork &

(sleep 15 && chromium) &
unclutter &
redshift &

(sleep 15 && xcompmgr) &

exec "$HOME/.local/bin/xmonad"
