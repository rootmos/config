#!/bin/sh

xrdb ~/.Xdefaults

xautolock -time 10 -locker "sudo pm-suspend" &

~/bin/dv

display.py --verbose --syslog &

exec xmonad
