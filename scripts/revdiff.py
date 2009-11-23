#!/usr/bin/env python

"""
This script runs a diff program (meld2.4 is the default) on different revisions
of a file in a bazaar repository.
"""

import os
import sys
import shutil
import tempfile
from optparse import OptionParser

parser = OptionParser()
parser.add_option("-r", "--revision", action="append", type="string", dest='revnos', 
                  help="revision number") 
parser.add_option("-d", "--differ", action="store", type="string", dest='differ',
                  help="diff program", default="meld2.4") 

(options, args) = parser.parse_args()

revnos = options.revnos

if len(args) == 0:
    parser.print_help()
    sys.exit(-1)

if len(options.revnos) > 3:
    print "revdiff can handle 3 revision numbers:  you provided %s" % len(revnos)
    sys.exit(-1)
        
# create a temp directory to hold the files
dirname = tempfile.mkdtemp()

# if no revision numbers given, use penultimate and latest
if len(revnos) == 0:
    revnos.extend([-2, -1])
elif len(revnos) == 1:  # diff given revno with latest
    revnos.append(-1)
    
try:
    cmd2 = options.differ
    for num in revnos:
        name = os.path.join(dirname, args[0] + ".%s" % num)
        cmd2 = cmd2 + ' ' + name
        cmd = "bzr cat -r %s" % num + " " + args[0] + " > " + name
        os.system(cmd)

    # perform the diff
    os.system(cmd2)
finally:
    shutil.rmtree(dirname)

