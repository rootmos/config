#!/bin/bash

curl -s 'https://www.archlinux.org/mirrorlist/?country=DE&country=SE&protocol=https' \
    | sed 's/^#Server/Server/' \
    | rankmirrors -n 10 - \
    | sudo install --backup /dev/stdin /etc/pacman.d/mirrorlist
