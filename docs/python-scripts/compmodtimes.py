#!/bin/env python

import os
import sys


def compmodtimes(f1, f2):
    if os.path.exists(f1):
        t1 = os.stat(f1).st_mtime
    else:
        t1 = -1

    if os.path.exists(f2): 
        t2 = os.stat(f2).st_mtime
    else:
        t2 = -1

    if t1 < t2:
       return -1
    elif t1 == t2:
       return 0
    else:  # t1 < t2
       return 1



if __name__ == "__main__":
    exit(compmodtimes(sys.argv[1], sys.argv[2]))
    
