#! /usr/bin/env python

"""
This prints out each line that you pass to it, prepending the elapsed time since the
last line came through.  It's intended as a quick and dirty way to provide timings
for tests in the output of things like openmdao test -v.  It's not super accurate, but
it'll give you a ballpark idea of which tests are taking a long time.

Note that if the source of the lines is sending them to stderr, you'll have to
account for that when piping them into linetimer, e.g.,

openmdao test -v |& linetimer.py

"""

import time
import sys
import os

def add_time(line, last):
    return "(%s) %s" % (time.time()-last, line)

def main(inp, output):
    t = time.time()
    while True:
        line = sys.stdin.readline()
        if not line:
            break
        output.write(add_time(line, t))
        t = time.time()
        
if __name__ == '__main__':
    os.environ['PYTHONUNBUFFERED'] = 'yes!'
    main(sys.stdin, sys.stdout)
