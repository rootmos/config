#!/usr/bin/env python

import subprocess
import logging

def connectedDisplays():
    xrandr = subprocess.Popen(["xrandr"], stdout=subprocess.PIPE)
    grep = subprocess.Popen(["grep", "-Po", "^\S+(?= connected)"], stdin=xrandr.stdout, stdout=subprocess.PIPE)
    xrandr.stdout.close()

    output, _ = grep.communicate()
    assert grep.returncode == 0

    displays = output.splitlines()
    logging.debug("Connected displays: %s" % displays)

    return displays

def connectDisplay(display):
    assert display
    logging.debug("Connecting display: %s" % display)
    subprocess.check_call(["xrandr", "--output", display, "--auto"])
    logging.info("Connected display: %s" % display)

def disconnectDisplay(display):
    assert display
    logging.debug("Disconnecting display: %s" % display)
    subprocess.check_call(["xrandr", "--output", display, "--off"])
    logging.info("Disconnected display: %s" % display)

def choose(choices, caseInsensitive=True):
    cmd = ["dmenu"]
    if caseInsensitive:
        cmd.append("-i")

    dmenu = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=subprocess.PIPE)

    inputForDmenu = "\n".join(choices)
    output, _ = dmenu.communicate(input=inputForDmenu)
    trimmedOutput = output.strip()

    assert dmenu.returncode == 0
    return trimmedOutput

def switchToDisplay(display):
    connectDisplay(display)
    for other in connectedDisplays():
        if other != display:
            disconnectDisplay(other)

switchToDisplay(choose(connectedDisplays()))
