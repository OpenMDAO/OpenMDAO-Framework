"""
Just an external program for testing.
"""

import sys
import time


if len(sys.argv) == 2:
    time.sleep(float(sys.argv[1]))
    sys.exit(0)
else:
    print "usage: python sleep.py nseconds"
    sys.exit(1)

