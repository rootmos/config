#!/bin/sh

set -o errexit

DIR=$(mktemp -d)
trap 'rm -rf $DIR' EXIT

TARGET=$DIR/$1.gpg
wget -O $TARGET http://46.101.178.30:4000/$1.gpg
gpg --batch --passphrase-file ~/.config/publish-passphrase --output $1 --decrypt $TARGET
