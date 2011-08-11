"""
This module contains everything necessary to install, activate, and test
an OpenMDAO release or development environment.
"""

import sys
import os
import shutil
import subprocess
from optparse import OptionParser


def activate_and_test(envdir, testargs=()):
    """"
    Runs the test suite on an OpenMDAO virtual environment located
    in the specified directory.
    
    Returns the return code of the process that runs the test suite.
    """
    if sys.platform.startswith('win'):
        devbindir = 'Scripts'
        command = 'activate.bat && openmdao_test %s' % ' '.join(testargs)
    else:
        devbindir = 'bin'
        command = '. ./activate && openmdao_test %s' % ' '.join(testargs)
        
    # activate the environment and run tests
    devbinpath = os.path.join(envdir, devbindir)
    os.chdir(devbinpath)
    print("running tests from %s" % devbinpath)
    env = os.environ.copy()
    for name in ['VIRTUAL_ENV','_OLD_VIRTUAL_PATH','_OLD_VIRTUAL_PROMPT']:
        if name in env: 
            del env[name]
    proc = subprocess.Popen(command, shell = True, cwd = os.getcwd(), env=env)
    proc.wait()
    return proc.returncode
    

if __name__ == '__main__':
    parser = OptionParser(usage="%prog [OPTIONS] -- testargs")
    parser.add_option("--pyversion", action="store", type='string', 
                      dest='pyversion', default="python", 
                      help="python version to use, e.g., 'python2.6'")
    parser.add_option("-d","--dir", action="store", type='string', 
                      dest='directory', 
                      help="virtual environment directory")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    startdir = os.getcwd()
    envdir = options.directory
    
    retcode = -1
            
    if '.' in options.pyversion:
        parts = options.pyversion.split('.')
        if len(parts) > 2:
            print "For python version, use only major.minor version numbers, e.g., ",
            print "'python2.6' not 'python2.6.5'"
            sys.exit(retcode)

    try:
        retcode = activate_and_test(envdir, testargs=args)
        print 'return code from test was %s' % retcode
    finally:
        os.chdir(startdir)

    sys.exit(retcode)
    