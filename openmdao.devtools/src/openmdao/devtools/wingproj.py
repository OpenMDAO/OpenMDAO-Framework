
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
    config.set('user attributes', 'proj.pyexec', 
               dict({None: ('custom', sys.executable)}))
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

def _find_wing():
    if sys.platform == 'win32':
        wname = 'wing.exe'
        tdir = r'C:\Program Files (x86)'
        try:
            locs = [os.path.join(tdir, p) for p in 
                    fnmatch.filter(os.listdir(tdir), 'WingIDE ?.?')]
        except:
            locs = []
    elif sys.platform == 'darwin':
        wname = 'wing'
        locs = ['/Applications/WingIDE.app/Contents/MacOS',
                '/Applications/Wing/WingIDE.app/Contents/MacOS']
    else:
        wname = 'wing?.?'
        locs = ['/usr/bin', '/usr/sbin', '/usr/local/bin']
        
    try:
        pathvar = os.environ['PATH']
    except KeyError:
        pathvar = ''
    
    all_locs = [p for p in pathvar.split(os.pathsep) if p.strip()] + locs
    for path in all_locs:
        try:
            matches = fnmatch.filter(os.listdir(path), wname)
        except:
            continue
        if matches:
            return os.path.join(path, sorted(matches)[-1])
        
    raise OSError("%s was not found in PATH or in any of the common places." %
                  wname)

def run_wing():
    """Runs the Wing IDE using our template project file."""
    wingpath = None
    projpath = ''
    for arg in sys.argv[1:]:
        if arg.startswith('--wingpath='):
            wingpath = arg.split('=')[1]
        elif arg.startswith('--proj='):
            projpath = arg.split('=')[1]
            
    if not os.path.isfile(projpath):
        venvdir = os.path.dirname(os.path.dirname(sys.executable))
        projpath = os.path.join(venvdir, 'etc', 'wingproj.wpr')
        
    # in order to find all of our shared libraries,
    # put their directories in LD_LIBRARY_PATH
    env = os.environ
    if sys.platform != 'win32':
        libs = env.get('LD_LIBRARY_PATH','').split(os.pathsep)
        rtop = find_up('.git')
        if not rtop:
            rtop = find_up('.git')
        if rtop:
            rtop = os.path.dirname(rtop)
            sodirs = set([os.path.dirname(x) for x in find_files(rtop,'*.so')])
            libs.extend(sodirs)
            env['LD_LIBRARY_PATH'] = os.pathsep.join(libs)
            
    if sys.platform == 'darwin':
        _modify_wpr_file(projpath) # have to put virtualenv sys path info in wing project file on Mac
        cmd = ['open', '-a', projpath]
    else:
        if not wingpath:
            wingpath = _find_wing()
        cmd = [wingpath, projpath]
    try:
        Popen(cmd, env=env)
    except Exception as err:
        print "Failed to run command '%s'." % ' '.join(cmd)
    
if __name__ == '__main__':
    run_wing()
