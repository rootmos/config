#!/bin/sh

set -o errexit

dv
touchpad disable

xrdb ~/.Xdefaults

chromium &
redshift &
unclutter -idle 2 &

exec xmonad
