#!/bin/sh

./configure --prefix=$HOME/root/vim \
    --enable-pythoninterp --enable-python3interp \
    --enable-rubyinterp

make -j4 install
