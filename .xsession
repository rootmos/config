#!/bin/sh

keymap code
xset m 0 0
xset s off -dpms
hsetroot -solid black

autocutsel -fork &
autocutsel -selection PRIMARY -fork &

unclutter &

while true; do
    "$HOME/.local/bin/xmonad-x86_64-linux"
    sleep 5
done
