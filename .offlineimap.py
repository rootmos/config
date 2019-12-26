#!/usr/bin/env python2
from subprocess import check_output

def load_pass(f):
    s = check_output("gpg -dq " + f, shell=True)
    for l in s.split('\n'):
        [k, v] = l.split('=')
        if k.endswith("imap_pass"):
            return v

def fastmail_pass():
    return load_pass("~/.password-store/fastmail/pass.rc.gpg")
