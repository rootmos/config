#!/bin/sh

xrdb ~/.Xdefaults

xautolock -time 10 -locker slock &

~/bin/dv

display.py --verbose --syslog &

exec xmonad
