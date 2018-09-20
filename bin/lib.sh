#!/bin/sh

pretty() {
    [ "$1" -eq 0 ] && echo ${2-OK} || echo ${3-ERR}
}

# usage: is_unlocked PARTITION_NAME
is_unlocked() {
    udevadm info /dev/mapper/$1 &> /dev/null
}

# usage: is_mounted BLOCK_DEVICE
is_mounted() {
    findmnt --noheadings $1 &> /dev/null
}

get_passphrase() {
    pinentry-gtk-2 <<EOF | grep -v "^OK" | grep "^D " | tail -c+3
SETPROMPT ${1-PIN}
SETOK ${2-OK}
GETPIN
EOF
}

is_yubikey_present() {
    lsusb | grep -q Yubikey
}
