#!/usr/bin/env python

"""
This is a helper script to make merging with bazaar and meld be more similar to the
graphical clearcase merge tool.  For a given file that is merged, e.g., file_x, the
bazaar merge will create file_x.BASE, file_x.OTHER, and file_x.THIS in the event of a
conflict. These three files are used by this script to perform the graphical merge.

The user will have the option to update the file_x.OTHER or the file_x.THIS version 
of file_x, and will be prompted after the graphical merge to determine which one
should be used to overwrite file_x.


usage: gmerge.py filename

"""

import os
import sys
import difflib
import shutil
import tempfile
import stat
from optparse import OptionParser


def temp_copy(fname):
    tmp = tempfile.NamedTemporaryFile()
    tmp_name = tmp.name
    tmp.close()
    shutil.copy(fname, tmp_name)
    return tmp_name


def has_diffs(fname1, fname2):
    file1 = open(fname1, 'r')
    lines1 = file1.readlines()
    file1.close()
    file2 = open(fname2, 'r')
    lines2 = file2.readlines()
    file2.close()
    diff = difflib.unified_diff(lines1, lines2, 'file1','file2',n=0) 
    return len(list(diff)) > 0


def gmerge(cmd_args):    
    parser = OptionParser()
    #parser.add_option("","--log", action="store", type="string", dest="log",
    #                  help="a file to log results to")
    (options, args) = parser.parse_args(cmd_args)

    if len(args) == 1:
        base_file = args[0]+'.BASE'
        other_file = args[0]+'.OTHER'
        this_file = args[0]+'.THIS'
    else:
        usage()
        sys.exit(-1)    

    # write protect base file to avoid confusion
    base_perm = os.stat(base_file)[stat.ST_MODE]
    os.chmod(base_file, stat.S_IREAD) 

    # save a temp copy of the OTHER file
    tmp_other_file = temp_copy(other_file)

    # save a temp copy of the THIS file
    tmp_this_file = temp_copy(this_file)

    cmd = 'meld2.4 '+base_file+' '+other_file+' '+this_file
    os.system(cmd)

    # put back original permissions
    os.chmod(base_file, base_perm)

    updated = []
    if has_diffs(other_file, tmp_other_file):
        updated.append(other_file)
    if has_diffs(this_file, tmp_this_file):
        updated.append(this_file)
    else:
        updated = []

    undo = False
    if len(updated) != 1:    
        var = raw_input('Resolve file '+args[0]+' using (O)THER, (T)HIS, or (U)NDO? ')
        if var == 'o' or var == 'O':
            fname = other_file
        elif var == 't' or var == 'T':
            fname = this_file
        else:
            undo = True
    else: # updated has one entry, so only one of the files was updated
        fname = updated[0]
        var = raw_input('Resolve file '+args[0]+' using '+updated[0]+'? (y/n)')
        if var != 'y' and var != 'Y':
            undo = True

    if undo is True:    
        print 'resolve skipped - no changes made to '+args[0]
        if other_file in updated:
            print 'undoing changes to '+other_file
            shutil.copy(tmp_other_file, other_file)
        if this_file in updated:
            print 'undoing changes to '+this_file
            shutil.copy(tmp_this_file, this_file)
    else:
        print 'Replacing '+args[0]+' with '+fname
        shutil.copy(fname, args[0])
        print 'Resolving '+args[0]
        os.system('bzr resolve '+args[0])

    os.remove(tmp_other_file)
    os.remove(tmp_this_file)


if __name__ == '__main__':
    gmerge(sys.argv)

