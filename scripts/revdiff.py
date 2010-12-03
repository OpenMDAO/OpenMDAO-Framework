#!/usr/bin/env python

"""
This script runs a diff program (meld is the default) on different revisions
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
                  help="diff program", default="meld") 

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
    cmd = options.differ
    for num in revnos:
        name = os.path.join(dirname, "%s.%s" % (args[0], num))
        cmd = cmd + ' ' + name
        os.system("bzr cat -r %s %s > %s" % (num, args[0], name))

    # perform the diff
    os.system(cmd)
finally:
    shutil.rmtree(dirname)

