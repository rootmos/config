#!/bin/sh

xrdb ~/.Xdefaults

~/bin/dv

touchpad disable

xsettingsd &
auto-suspend &

systemctl --user import-environment DISPLAY XAUTHORITY

exec xmonad
