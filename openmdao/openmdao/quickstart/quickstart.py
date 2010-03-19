import os
import sys
import shutil
import os.path
from subprocess import Popen, STDOUT, check_call

def run(install_dir=None):
    """Set up the OpenMDAO environment in the specified directory. The 
    directory must NOT exist prior to calling this function.
    """
    if install_dir is None:
        install_dir = sys.argv[1]
    startdir = os.getcwd()
    os.makedirs(install_dir)
    path = os.path.abspath(install_dir)
    try:
        bstrapfname = os.path.join(os.path.dirname(__file__), 
                                   'isolated_bootstrap.py')
        bconfigfname = os.path.join(os.path.dirname(__file__), 
                                    'buildout.cfg')
        os.chdir(path)
        shutil.copy(bconfigfname, os.path.join(path, 'buildout.cfg'))
        check_call([sys.executable, ' %s' % bstrapfname])
    finally:
        os.chdir(startdir)