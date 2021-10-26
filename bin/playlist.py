#!/usr/bin/env python3

import argparse
import logging
import re
import itertools

class List:
    def __init__(self, fn, offset=None):
        logger.info(f"loading list: fn={fn} offset={offset}")

        self.entries = []
        with open(fn, "r") as f:
            for e in f:
                self.entries.append(e.rstrip("\n"))

        self.offset = offset or 0

    pattern = re.compile(r"^(.*?)(?:\+(\d+))?$")

    def __len__(self):
        return len(self.entries)

    @staticmethod
    def load(s):
        ms = List.pattern.match(s)
        fn = ms.group(1)
        offset = ms.group(2)
        if offset is not None:
            offset = int(offset)
        return List(fn=fn, offset=offset)

    def __iter__(self):
        return iter(self.entries[self.offset:])

    def cycle(self):
        return self.entries[self.offset:] + self.entries[:self.offset]

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

def parse_args():
    parser = argparse.ArgumentParser(
            description="The one playlist script to play them all",
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument("--log", default="WARN", help="set log level")
    parser.add_argument("--log-file", metavar="FILE", default=None, help="redirect stdout and stderr to FILE")

    parser.add_argument("--interleave", action="store_true")
    parser.add_argument("--cycle", action="store_true")

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

    try:
        if args.interleave:
            n = max([len(l) for l in ls])
            iters = [itertools.cycle(l.cycle()) for l in ls]
            for i in range(n):
                for j in iters:
                    print(next(j))
        else:
            for l in ls:
                if args.cycle:
                    for e in l.cycle():
                        print(e)
                else:
                    for e in l:
                        print(e)
    except BrokenPipeError:
        pass
