#!/usr/bin/env python3

import argparse
import itertools
import logging
import random
import re
import subprocess
import os

class Entry:
    def __init__(self, fn, prefix=None):
        self.fn = fn
        self.prefix = prefix
        self._duration = None

    def __str__(self):
        if self.prefix is not None:
            return os.path.join(self.prefix, self.fn)
        else:
            return self.fn

    @property
    def duration(self):
        if self._duration is None:
            ls = subprocess.check_output(["duration", "-s", self.fn]).splitlines()
            self._duration = float(ls[0])
        return self._duration


class List:
    def __init__(self, fn, offset=None, shuffle=None, roll=None):
        prefix = None
        fn = os.path.realpath(fn)
        root_marker = os.path.join(os.path.dirname(fn), ".root")
        if os.path.exists(root_marker):
            with open(root_marker, "r") as f:
                root = f.readlines()[0].rstrip("\n")
                prefix = os.path.realpath(os.path.join(os.path.dirname(root_marker), root))

        logger.info(f"loading list: fn={fn} offset={offset} shuffle={shuffle} prefix={prefix}")

        self.entries = []
        with open(fn, "r") as f:
            for e in f:
                self.entries.append(Entry(e.rstrip("\n"), prefix=prefix))

        if roll:
            i = random.randint(1, len(self.entries))
            self.entries = self.entries[i:] + self.entries[:i]
        elif shuffle:
            random.shuffle(self.entries)

        self.offset = offset or 0
        self._duration = None
        logger.debug(f"{self.offset}")

    pattern = re.compile(r"^(.*?)(?:\:(\d+|z|r))?$")

    def __len__(self):
        return len(self.entries)

    @staticmethod
    def load(s):
        ms = List.pattern.match(s)
        fn = ms.group(1)
        mode = ms.group(2)

        shuffle = False
        roll = False
        offset = None
        if mode == "z":
            shuffle = True
        elif mode == "r":
            roll = True
        elif mode is not None:
            offset = int(mode)

        return List(fn=fn, offset=offset, shuffle=shuffle, roll=roll)

    def __iter__(self):
        return iter(self.entries[self.offset:])

    def cycle(self):
        return self.entries[self.offset:] + self.entries[:self.offset]

    @property
    def duration(self):
        if self._duration is None:
            self._duration = sum(e.duration for e in self)
        return self._duration

logger = None
def setup_logger(level):
    f = logging.Formatter(
        fmt="%(asctime)s:%(name)s:%(levelname)s %(message)s",
        datefmt="%Y-%m-%dT%H:%M:%S%z")
    ch = logging.StreamHandler()
    ch.setFormatter(f)
    l = logging.getLogger(logger)
    l.setLevel(level)
    l.addHandler(ch)
    return l

def interleave(ls):
    n = max([len(l) for l in ls])
    iters = [itertools.cycle(l.cycle()) for l in ls]
    for i in range(n):
        for j in iters:
            yield next(j)

def cycle(ls):
    for l in ls:
        if args.cycle:
            for e in l.cycle():
                yield e

def take_duration(i, duration):
    d = 0.0
    for e in i:
        yield e
        d += e.duration
        if d >= duration:
            break

def parse_args():
    parser = argparse.ArgumentParser(
            description="The one playlist script to play them all",
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument("--log", default="WARN", help="set log level")
    parser.add_argument("--log-file", metavar="FILE", default=None, help="redirect stdout and stderr to FILE")

    parser.add_argument("--interleave", action="store_true")
    parser.add_argument("--cycle", action="store_true")

    parser.add_argument("--head", metavar="N", type=int, default=None)
    parser.add_argument("--duration", metavar="HOURS", type=float, default=None)

    parser.add_argument("list", nargs="+")

    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()

    if args.log_file is not None:
        sys.stderr = sys.stdout = open(args.log_file, "a")
    logger = setup_logger(args.log.upper())
    logger.debug(f"args: {args}")

    ls = []
    for fn in args.list:
        ls.append(List.load(fn))

    i = None
    if args.interleave:
        i = interleave(ls)
    else:
        if args.cycle:
            i = cycle(ls)
        else:
            i = itertools.chain(*ls)

    if args.head is not None:
        i = itertools.islice(i, args.head)

    if args.duration is not None:
        i = take_duration(i, args.duration*3600)

    try:
        for e in i:
            print(str(e))
    except BrokenPipeError:
        pass
