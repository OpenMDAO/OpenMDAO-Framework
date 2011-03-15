
import os
import os.path
import sys
import fnmatch
import logging
from subprocess import Popen
import ConfigParser

from openmdao.util.fileutil import find_in_path, find_in_dir_list, find_files, find_up

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
               dict({None: ('custom',os.pathsep.join(sys.path))}))
    with open(fpath, 'w') as fp:
        config.write(fp)
    f = open(fpath,'r')
    content = f.read()
    f.close()
    
    f = open(fpath, 'w')
    f.write('#!wing\n#!version=3.0\n')
    f.write(content)
    f.close()
    
def run_wing():
    """Runs the Wing IDE using our template project file."""
    wingpath = None
    projpath = ''
    for arg in sys.argv[1:]:
        if arg.startswith('--wingpath='):
            wingpath = arg.split('=')[1]
        elif arg.startswith('--proj='):
            projpath = arg.split('=')[1]
    if not wingpath:
        if sys.platform == 'win32':
            wname = 'wing.exe'
            locs = [r'C:\Program Files (x86)\WingIDE 3.2']
        elif sys.platform == 'darwin':
            wname = 'wing'
            locs = ['/Applications/WingIDE.app/Contents/MacOS',
                    '/Applications/Wing/WingIDE.app/Contents/MacOS']
        else:
            wname = 'wing3.2'
            locs = ['/usr/bin', '/usr/sbin', '/usr/local/bin']
            
        wingpath = find_in_path(wname) # searches PATH
        if not wingpath:
            wingpath = find_in_dir_list(wname, locs) # look in common places
        if not wingpath:
            raise OSError("%s was not found in PATH or in any of the common places." %
                          wname)

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
        bzrtop = find_up('.bzr')
        if bzrtop:
            bzrtop = os.path.dirname(bzrtop)
            sodirs = set([os.path.dirname(x) for x in find_files(bzrtop,'*.so')])
            libs.extend(sodirs)
            env['LD_LIBRARY_PATH'] = os.pathsep.join(libs)
    
    try:
        Popen([wingpath, projpath], env=env)
    except Exception as err:
        print 'Failed to run wing executable (%s) using project (%s).' % (wingpath, projpath)
    
if __name__ == '__main__':
    run_wing()
