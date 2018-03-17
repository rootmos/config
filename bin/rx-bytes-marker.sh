#!/bin/bash

set -o errexit

I=
SET=
while getopts "si:" opt; do
    case $opt in
        s) SET=1
           ;;
        i) I=$OPTARG ;;
        \?) echo "Invalid option: -$OPTARG" >&2
            exit 2 ;;
    esac
done

if [ -z "$I" ]; then
    echo "no interface selected" >&2
    exit 2
fi

mf=$HOME/.rx-bytes-marker-$I
c=$(cat /sys/class/net/$I/statistics/rx_bytes)

if [ -n "$SET" ]; then
    echo $c > $mf
else
    d=
    if [ -f $mf ]; then
        d="($((($c - $(cat $mf)) / 1048576)))"
    fi

    echo $(($c / 1048576))${d}MiB
fi
