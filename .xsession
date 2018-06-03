#!/bin/sh

set -o errexit

dv
touchpad disable
xset m 0 0

xrdb ~/.Xdefaults
xrdb -merge ~/git/config/solarized.light

chromium &
redshift &
unclutter &

exec xmonad
