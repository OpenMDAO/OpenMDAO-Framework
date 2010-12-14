#!/usr/local/bin/python2.5

"""
This is a helper script to make merging with bazaar and meld be more similar to
the graphical clearcase merge tool.  For a given file that is merged, e.g.,
file_x, the bazaar merge will create file_x.BASE, file_x.OTHER, and file_x.THIS
in the event of a conflict. These three files are used by this script to perform
the graphical merge.

The user will have the option to update the file_x.OTHER or the file_x.THIS
version  of file_x, and will be prompted after the graphical merge to determine
which one should be used to overwrite file_x.

usage: conrez.py filename

"""

import os
import sys
import difflib
import shutil
import tempfile
import stat
import logging
from optparse import OptionParser
from subprocess import Popen,PIPE,STDOUT

import wx

# we need this simple App class to be instantiated before we can do any
# wx stuff, even a simple message dialog
class GmergeApp(wx.App):       
    def OnInit(self):
        """This gets called on initialization of the class"""
        return True
        

logger = logging.getLogger('')

def run_command(cmd, sh=True):
    """Run a command using Popen and return its output (stdout and stderr)
    and its return code as a tuple. If the command is a python file, prepend
    python to the command string
    """
    p = Popen(cmd, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=sh)
    output = p.communicate()[0]
    return (output, p.returncode)


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


def get_repo_dir():
    """return the top directory of this bazaar repository"""
    abspath = os.path.abspath(os.getcwd())    

    
def gmerge(conflict_file, merge_tool):
    """Perform a graphical merge using the specified merge tool, then
    call bzr resolve on the merged file if confirmed by the user.
    """ 
    
    logger.info('merging file '+conflict_file+' manually')
    
    base_file = conflict_file+'.BASE'
    if not os.path.isfile(base_file):
        base_file = None
    other_file = conflict_file+'.OTHER'
    this_file = conflict_file+'.THIS'

    # write protect base file to avoid confusion
    if base_file:
        base_perm = os.stat(base_file)[stat.ST_MODE]
        os.chmod(base_file, stat.S_IREAD) 

    try:
        # save a temp copy of the OTHER file
        tmp_other_file = temp_copy(other_file)

        # save a temp copy of the THIS file
        tmp_this_file = temp_copy(this_file)

        if base_file:
            cmd = merge_tool+' '+base_file+' '+other_file+' '+this_file
        else:
            cmd = merge_tool+' '+other_file+' '+this_file
        merge_output,merge_return = run_command(cmd)
    finally:
        # put back original permissions
        if base_file:
            os.chmod(base_file, base_perm)

    updated = []
    if has_diffs(other_file, tmp_other_file) is True:
        updated.append(other_file)
    if has_diffs(this_file, tmp_this_file) is True:
        updated.append(this_file)

    undo = False
    if len(updated) != 1:  
        try:  
            choices=[(other_file,' (from merged branch)'), 
                     (this_file,'     (from this branch)')]
            short_choices = [os.path.split(x[0])[1]+x[1] for x in choices]
            dialog = wx.SingleChoiceDialog(None, "Choose File to Update From",
                                           ' ', 
                                           choices=short_choices)
            result = dialog.ShowModal()
            if result == wx.ID_OK:
                idx = dialog.GetSelection()
                fname = choices[idx][0]
            else:
                fname = ''
                undo = True
            dialog.Destroy()
        except Exception, err:
            print str(err)
            logger.error(str(err))
            undo = True
    else: # updated has one entry, so only one of the files was updated
        fname = updated[0]
        try:
            dialog = wx.MessageDialog(None, 
                              'Resolve file '+os.path.split(conflict_file)[1]+
                              ' using '+os.path.split(updated[0])[1]+'?', 
                              ' ', style=wx.YES_NO|wx.YES_DEFAULT)
            result = dialog.ShowModal()
            dialog.Destroy()
        except Exception, err:
            print str(err)
            logger.error(str(err))
            undo = True
        else:    
            if result == wx.ID_NO:
                undo = True
        

    if undo is True:    
        logger.info('resolve skipped - no changes made to '+conflict_file)
        if other_file in updated:
            logger.info('undoing changes to '+other_file)
            shutil.copy(tmp_other_file, other_file)
        if this_file in updated:
            logger.info('undoing changes to '+this_file)
            shutil.copy(tmp_this_file, this_file)
    else:
        logger.info('Replacing '+conflict_file+' with '+fname)
        shutil.copy(fname, conflict_file)
        logger.info('Resolving '+conflict_file)
        os.system('bzr resolve '+conflict_file)

    os.remove(tmp_other_file)
    os.remove(tmp_this_file)


def merge_all(merge_tool):
    startdir = os.getcwd()
    path = os.path.abspath('.')
    rest = path
    
    while rest != '':
        os.chdir(path)
        if os.path.exists('.bzr'):
            break
        else:
            path,rest = os.path.split(path)
    else:
        logger.error('not running from within a repository. exiting.')
        os.chdir(startdir)
        raise SystemExit(-1)

    try:        
        out, ret = run_command('bzr conflicts')
        for line in out.splitlines():
            if line.startswith('Text conflict'):
                name = line.split()[3]
                gmerge(name, merge_tool)
    finally:
        os.chdir(startdir)


if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-f", "", action="store", type="string", dest="fname",
                      help="specify name of file to merge manually")
    parser.add_option("--log", action="store", type="string", dest="log",
                      help="specify log file name (defaults to gmerge.log)",
                      default='gmerge.log')
    parser.add_option("--using", action="store", type="string", dest="merge_tool",
                      help="specify the merge tool to use",
                      default='meld')
    (options, args) = parser.parse_args(sys.argv[1:])


    # set up logging to file 
    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(name)s %(levelname)s %(message)s',
                        datefmt='%H:%M',
                        filename=options.log,
                        filemode='w')
                        
    logger = logging.getLogger('')
    
    # define a Handler which writes INFO messages or higher to the sys.stderr
    console = logging.StreamHandler()
    console.setLevel(logging.DEBUG)
    # set a format which is simpler for console use
    formatter = logging.Formatter('%(message)s')
    # tell the handler to use this format
    console.setFormatter(formatter)
    # add the handler to the root logger
    logger.addHandler(console)
    
    app = GmergeApp(redirect = False)
    
    if options.fname:
        gmerge(options.fname, options.merge_tool)
    else:
        merge_all(options.merge_tool)

