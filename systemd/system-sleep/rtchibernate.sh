#!/bin/bash

curtime=$(date +%s)
autohibernate=7200

lock=/var/run/systemd/rtchibernate.lock

if [ "$1" = "pre" -a "$2" = "suspend" ]
then
    # Suspending.  Record current time, and set a wake up timer.
    echo "$curtime" > $lock
    rtcwake -m no -s $autohibernate
fi

if [ "$1" = "post" -a "$2" = "suspend" ]
then
    # Coming out of sleep
    sustime=$(cat $lock)
    rm $lock
    # Did we wake up due to the rtc timer above?
    if [ $(($curtime - $sustime)) -ge $autohibernate ]
    then
        # Then hibernate
        /usr/bin/env systemctl hibernate
    else
        # Otherwise cancel the rtc timer and wake up normally.
        rtcwake -m no -s 1
    fi
fi
