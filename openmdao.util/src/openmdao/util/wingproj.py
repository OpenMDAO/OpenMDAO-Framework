
import os
import os.path
import sys
import fnmatch
import logging
from subprocess import Popen

def _find_files_and_dirs(pat, startdir):
    for path, dirlist, filelist in os.walk(startdir):
        for name in fnmatch.filter(filelist+dirlist, pat):
            yield os.path.join(path, name)

def _find_files(pat, startdir):
    for path, dirlist, filelist in os.walk(startdir):
        for name in fnmatch.filter(filelist, pat):
            yield os.path.join(path, name)

def _find_bzr(path=None):
    if not path:
        path = os.getcwd()
    if not os.path.exists(path):
        return None
    while path:
        if os.path.exists(os.path.join(path, '.bzr')):
            return path
        else:
            pth = path
            path = os.path.dirname(path)
            if path == pth:
                return None
    return None

def run_wing():
    """Runs the Wing IDE after first setting environment variables
    necessary to locate shared libraries.
    """
    wingpath = None
    projpath = ''
    for arg in sys.argv[1:]:
        if arg.startswith('--wingpath='):
            wingpath = arg.split('=')[1]
        elif arg.startswith('--proj='):
            projpath = arg.split('=')[1]
    if not wingpath:
        if sys.platform == 'win32':
            wingpath = 'wing.exe'
        elif sys.platform == 'darwin':
            wingpath = '/Applications/Wing/WingIDE.app/Contents/MacOS/wing'
        else:
            wingpath = 'wing3.2'
    if not os.path.isfile(projpath):
        venvdir = os.path.dirname(os.path.dirname(sys.executable))
        projpath = os.path.join(venvdir, 'etc', 'wingproj.wpr')
        if not os.path.isfile(projpath):
            projpath = ''
        
    # in order to find all of our shared libraries,
    # put their directories in LD_LIBRARY_PATH
    env = os.environ
    if sys.platform != 'win32':
        libs = env.get('LD_LIBRARY_PATH','').split(os.pathsep)
        bzrtop = _find_bzr()
        if bzrtop:
            sodirs = set([os.path.dirname(x) for x in _find_files('*.so',bzrtop)])
            libs.extend(sodirs)
            env['LD_LIBRARY_PATH'] = os.pathsep.join(libs)
            
    Popen([wingpath, projpath], env=env)
