#!/bin/sh

set -o errexit

cd ~/.vim/bundle/command-t/ruby/command-t/ext/command-t

make clean
ruby extconf.rb
make
