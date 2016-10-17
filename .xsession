#!/bin/sh

xrdb ~/.Xdefaults

~/bin/dv

touchpad disable

xsettingsd &

systemctl --user import-environment DISPLAY XAUTHORITY

exec xmonad
