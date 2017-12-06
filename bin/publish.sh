#!/bin/sh

DIR=$(mktemp -d)
trap 'rm -rf $DIR' EXIT

TARGET=$DIR/$1.gpg
gpg --batch --passphrase-file ~/.config/publish-passphrase --output $TARGET --symmetric $1
scp $TARGET drop:web
