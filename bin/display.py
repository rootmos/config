#!/usr/bin/env python

import subprocess
import logging
import logging.handlers
import argparse
import time

logger = logging.getLogger()

def connectedDisplays():
    xrandr = subprocess.Popen(["xrandr"], stdout=subprocess.PIPE)
    grep = subprocess.Popen(["grep", "-Po", "^\S+(?= connected)"], stdin=xrandr.stdout, stdout=subprocess.PIPE)
    xrandr.stdout.close()

    output, _ = grep.communicate()

    displays = output.splitlines()
    logger.debug("Connected displays: %s" % displays)
    return displays

def activeDisplays():
    xrandr = subprocess.Popen(["xrandr"], stdout=subprocess.PIPE)
    grep = subprocess.Popen(["grep", "-P", "[\d]+x[\d]+\+[\d]+\+[\d]+"], stdin=xrandr.stdout, stdout=subprocess.PIPE)
    xrandr.stdout.close()

    output, _ = grep.communicate()

    activeDisplayLines = output.splitlines()
    activeDisplays = []
    for line in activeDisplayLines:
        words = line.split(" ")
        display = words[0]
        activeDisplays.append(display)

    logger.debug("Active displays: %s" % activeDisplays)
    return activeDisplays

def primaryDisplay():
    xrandr = subprocess.Popen(["xrandr"], stdout=subprocess.PIPE)
    grep = subprocess.Popen(["grep", "primary"], stdin=xrandr.stdout, stdout=subprocess.PIPE)
    xrandr.stdout.close()

    output, _ = grep.communicate()
    assert grep.returncode == 0

    activeDisplayLines = output.splitlines()
    activeDisplays = []
    for line in activeDisplayLines:
        words = line.split(" ")
        display = words[0]
        return display

    logger.error("No primary display!")

def connectDisplay(display):
    assert display
    logger.debug("Connecting display: %s" % display)
    subprocess.check_call(["xrandr", "--output", display, "--auto"])
    logger.info("Connected display: %s" % display)

def disconnectDisplay(display):
    assert display
    logger.debug("Disconnecting display: %s" % display)
    subprocess.check_call(["xrandr", "--output", display, "--off"])
    logger.info("Disconnected display: %s" % display)

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
    logger.info("Switching to display: %s" % display)
    connectDisplay(display)
    for other in connectedDisplays():
        if other != display:
            disconnectDisplay(other)

class Context(object):
    def __init__(self):
        self.connectedDisplays = connectedDisplays()
        self.activeDisplays = activeDisplays()
        self.primaryDisplay = primaryDisplay()

class Rule(object):
    def isSatisfied(self, context):
        raise RuntimeError("Not implemented!")

    def action(self):
        raise RuntimeError("Not implemented!")

class IfNoActiveAndConnectedDisplaysSwitchToPrimary(Rule):
    def isSatisfied(self, context):
        result = not set(context.activeDisplays).intersection(context.connectedDisplays)
        logger.debug("Checking if there's no active and connected displays: %s" % result)
        return result

    def action(self, context):
        logger.debug("Switch to primary display: %s" % context.primaryDisplay)
        switchToDisplay(context.primaryDisplay)

rules = [IfNoActiveAndConnectedDisplaysSwitchToPrimary()]

def parseArguments():
    parser = argparse.ArgumentParser()
    parser.add_argument('--choose', action='store_true')
    parser.add_argument('--append', action='store_true')
    parser.add_argument('--remove', action='store_true')
    parser.add_argument('--debug', action='store_true')
    parser.add_argument('--verbose', action='store_true')
    parser.add_argument('--syslog', action='store_true')
    return parser.parse_args()

def setupLogging(args):
    setupLoggingLevel(args)
    setupLoggingHandels(args)

def setupLoggingLevel(args):
    level = logging.WARNING
    if args.verbose:
        level = logging.INFO
    if args.debug:
        level = logging.DEBUG
    logger.setLevel(level)

def setupLoggingHandels(args):
    if args.syslog:
        setupSyslogLogging()
    else:
        setupConsoleLogging()

def setupSyslogLogging():
    syslog = logging.handlers.SysLogHandler(address='/dev/log')
    logger.addHandler(syslog)

def setupConsoleLogging():
    console = logging.StreamHandler()
    logger.addHandler(console)

def doChooseMode():
    logger.debug("Executing in Choose mode")
    switchToDisplay(choose(connectedDisplays()))

def doAppendMode():
    logger.debug("Executing in Append mode")
    connectDisplay(choose(connectedDisplays()))

def doRemoveMode():
    logger.debug("Executing in Remove mode")
    disconnectDisplay(choose(connectedDisplays()))

def doDaemonMode():
    try:
        while True:
            executeRules()
            time.sleep(1)
    except KeyboardInterrupt:
        logger.debug("Stopping daemon")

def executeRules():
    logger.debug("Executing the rules")
    context = Context()
    for rule in rules:
        if rule.isSatisfied(context):
            rule.action(context)

if __name__ == "__main__":
    args = parseArguments()
    setupLogging(args)

    if args.choose:
        doChooseMode()
    elif args.append:
        doAppendMode()
    elif args.remove:
        doRemoveMode()
    else:
        doDaemonMode()
