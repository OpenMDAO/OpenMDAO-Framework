#!/usr/bin/env python

"""
This is a helper script to make merging with bazaar and meld be more
similar to the clearcase merge tool.  For a given file that is merged,
e.g., file_x, the bazaar merge will create file_x.BASE, file_x.OTHER,
and file_x.THIS. This tool checks for updates to the file_x.THIS file,
and if it changes, it gives the user the option to overwrite file_x with
the contents of file_x.THIS.  This tool also write protects the file_x.BASE
and file_x.OTHER files to avoid confusion.  If the three filenames passed 
don't follow the .BASE, .OTHER, .THIS format, then this will act as just a 
manual merge tool that will not overwrite any other file.
"""

import os
import sys
import difflib
import shutil
#from subprocess import Popen,PIPE,STDOUT

if sys.argv[1].endswith('.BASE') and sys.argv[2].endswith('.OTHER') and sys.argv[3].endswith('.THIS'):
    isBazaarMerge = True
else:
    isBazaarMerge = False

if isBazaarMerge is True:
    # get contents of the original local file
    forig = open(sys.argv[3],'r')
    orig_lines = forig.readlines()
    forig.close()

cmd = 'meld2.4 '+sys.argv[1]+' '+sys.argv[2]+' '+sys.argv[3]
#p = Popen(cmd, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
#output, ret = p.communicate()
os.system(cmd)

if isBazaarMerge is True:
    # write protect first two files to avoid confusion
#    os.chmod(sys.argv[1], stat.S_IRU) 
#    os.chmod(sys.argv[2], stat.S_IRU) 

    # get contents of the updated local file
    fnew = open(sys.argv[3],'r')
    new_lines = fnew.readlines()
    fnew.close()

    difflines = list(difflib.unified_diff(orig_lines, new_lines,n=0))

    if len(difflines) > 0 and sys.argv[3].endswith('.THIS'):
        var = raw_input('Resolve changes to '+sys.argv[3]+'? (y/n) ')

        if var == 'y' or var == 'Y':
            print 'Keeping changes'
            shutil.copy(sys.argv[3], sys.argv[3][:-5])
            os.system('bzr resolve '+sys.argv[3][:-5])
        else:
            print 'Change will not be recorded'


