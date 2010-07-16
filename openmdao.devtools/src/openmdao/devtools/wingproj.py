
import os
import os.path
import sys
import fnmatch
import logging
from subprocess import Popen
import ConfigParser

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

_LINE_WIDTH = 68

def _wingify(obj, left_margin=0):
    """Take an object, convert to a string, split it on commas. If any piece 
    is longer than 80 chars, split it up into smaller chunks. Finally, recombine 
    it all back into a string with each entry on a new line
    """
    flat = []
    parts = str(obj).strip().split(',')
    for idx, part in enumerate(parts):
        if len(part) < _LINE_WIDTH and idx < len(parts)-1:
            flat.append(part+',')
        else:
            part = part.strip()
            just = _LINE_WIDTH-left_margin
            numsubs = len(part)/just+1
            for i in range(0, numsubs):
                p = part[i*just:i*just+just]
                if not p.startswith("'"):
                    p = "'"+p
                p = left_margin*" "+p+"'\\"
                flat.append(p)
            flat[len(flat)-1] = flat[len(flat)-1][:-2]
    if " ')}" in flat[-1]:
        flat[-1] = flat[-1].replace(" ')}"," )}")
    return '\n'.join(flat)

def _modify_wpr_file(fpath):
    config = ConfigParser.ConfigParser()
    config.read(fpath)
    config.set('user attributes', 'proj.pypath', 
               _wingify(dict({None: ('custom',os.pathsep.join(sys.path))}), left_margin=18))
    with open(fpath, 'w') as fp:
        config.write(fp)

def run_wing():
    """Runs the Wing IDE after first setting up a profile containing
    the files in the openmdao repository. It also adds all of our 
    unit tests to the testing area.
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
            wingpath = '/Applications/WingIDE.app/Contents/MacOS/wing'
            if not os.path.exists(wingpath):
                wingpath = '/Applications/Wing/WingIDE.app/Contents/MacOS/wing'
        else:
            wingpath = 'wing3.2'
    if not os.path.isfile(projpath):
        venvdir = os.path.dirname(os.path.dirname(sys.executable))
        projpath = os.path.join(venvdir, 'etc', 'wingproj.wpr')
        
    if sys.platform == 'darwin':
        _modify_wpr_file(projpath) # have to put virtualenv sys path info in wing project file on Mac
        
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

    
if __name__ == '__main__':
    run_wing()
