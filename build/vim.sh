#!/bin/sh

set -o errexit

./configure --prefix=$HOME/root/vim \
    --enable-pythoninterp --enable-python3interp \
    --enable-rubyinterp

exec make -j4 clean install
