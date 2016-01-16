#!/usr/bin/env python

import subprocess
import logging
import logging.handlers
import argparse
import time
import os
import json

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


def connectAndDisconnectDisplays(connect = None, disconnect = None):
    if connect is None:
        connect = []
    if disconnect is None:
        disconnect = []

    if not connect and not disconnect:
        return

    executeCommands(preConnectCommands(connect))
    executeCommands(preDisconnectCommands(disconnect))

    logger.debug("Connecting: %s. Disconnecting: %s." % (connect, disconnect))
    subprocess.check_call(buildXRandrCommand(connect, disconnect))
    logger.info("Connected: %s. Disconnected: %s." % (connect, disconnect))

    while not set(connect).issubset(activeDisplays()):
        logger.debug("Wating for %s to become active..." % connect)
        time.sleep(1)

    executeCommands(postConnectCommands(connect))
    executeCommands(postDisconnectCommands(disconnect))


def buildXRandrCommand(connect, disconnect):
    cmd = ["xrandr"]
    for display in connect:
        cmd += ["--output", display, "--auto"]
    for display in disconnect:
        cmd += ["--output", display, "--off"]
    return cmd

def connectDisplay(display):
    connectAndDisconnectDisplays(connect=[display])

def disconnectDisplay(display):
    connectAndDisconnectDisplays(disconnect=[display])

def configuration():
    filename = os.path.expanduser('~/.config/displays.json')
    if not os.path.isfile(filename):
        return {}

    with open(filename) as f:
        data = f.read()
        return json.loads(data)

def configurationForDisplay(configuration, display):
    if display in configuration:
        return configuration[display]
    else:
        return {}

def getCommandsForDisplay(configuration, commandType, display):
    displayConfig = configurationForDisplay(configuration, display)
    if commandType in displayConfig:
        return displayConfig[commandType]
    else:
        return []

def getCommandsForAllDisplays(commandType, displays = None):
    conf = configuration()
    commands = getCommandsForDisplay(conf, commandType, "default")

    if displays:
        for display in displays:
            commands += getCommandsForDisplay(conf, commandType, display)

    logger.debug("Fetched commands for type %s: %s" % (commandType, commands))
    return commands

def preConnectCommands(displays = None):
    return getCommandsForAllDisplays("pre_connect", displays)
def postConnectCommands(displays = None):
    return getCommandsForAllDisplays("post_connect", displays)
def preDisconnectCommands(displays = None):
    return getCommandsForAllDisplays("pre_disconnect", displays)
def postDisconnectCommands(displays = None):
    return getCommandsForAllDisplays("post_disconnect", displays)

def executeCommands(commands):
    for command in commands:
        logger.debug("Executing command: %s" % command)
        returncode = subprocess.call(command, shell=True)
        logger.info("Command exited with code %d: %s" % (returncode, command))

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

    disconnect = set(activeDisplays())
    if display in disconnect:
        disconnect.remove(display)
    connectAndDisconnectDisplays(connect=[display], disconnect=disconnect)

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
