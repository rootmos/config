#!/bin/sh

dv
xset m 0 0
xset s off -dpms

chromium &

autocutsel -fork &
autocutsel -selection PRIMARY -fork &

exec $HOME/.xmonad/xmonad
