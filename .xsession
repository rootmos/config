#!/bin/sh

dv
xset m 0 0

xrdb ~/.Xdefaults
xrdb -merge ~/git/config/solarized.$(<$HOME/.theme)

chromium &
redshift &
unclutter -idle 2 &

autocutsel -fork &
autocutsel -selection PRIMARY -fork &

xset s off -dpms

exec xmonad
