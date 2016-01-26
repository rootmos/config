#!/bin/sh

xrdb ~/.Xdefaults

xautolock -time 10 -locker "systemctl suspend" &

~/bin/dv

exec xmonad
