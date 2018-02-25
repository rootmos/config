#!/bin/sh

set -o errexit

dv
touchpad disable

xrdb ~/.Xdefaults

chromium &
redshift &
unclutter &

exec xmonad
