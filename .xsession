#!/bin/sh

dv
xset m 0 0

xrdb ~/.Xdefaults
xrdb -merge ~/git/config/solarized.dark

chromium &
redshift &
unclutter &

autocutsel -fork &
autocutsel -selection PRIMARY -fork &

exec xmonad
