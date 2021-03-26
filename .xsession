#!/bin/sh

dv
xset m 0 0
xset s off -dpms
hsetroot -solid black

autocutsel -fork &
autocutsel -selection PRIMARY -fork &

(sleep 3 && chromium) &
unclutter &
redshift &
(sleep 3 && xcompmgr) &

exec "$HOME/.local/bin/xmonad"
