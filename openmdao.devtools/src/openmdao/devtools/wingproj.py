
import os
import os.path
import sys
import fnmatch
import logging
from subprocess import Popen
import ConfigParser
from optparse import OptionParser

from openmdao.util.fileutil import find_in_path, find_in_dir_list, find_files, find_up

def _modify_wpr_file(template, outfile, version='4.0'):
    config = ConfigParser.ConfigParser()
    config.read(template)
    if sys.platform == 'darwin':
        config.set('user attributes', 'proj.pyexec', 
                   dict({None: ('custom', sys.executable)}))
        config.set('user attributes', 'proj.pypath', 
                   dict({None: ('custom',os.pathsep.join(sys.path))}))
    
    with open(outfile, 'w') as fp:
        fp.write('#!wing\n#!version=%s\n' % version)
        config.write(fp)
        

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
    parser = OptionParser()
    parser.add_option("-w", "--wingpath", action="store", type="string", 
                      dest="wingpath", help="location of WingIDE executable")
    parser.add_option("-p", "--projpath", action="store", type="string", 
                      dest="projpath", default='',
                      help="location of WingIDE project file")
    parser.add_option("-v", "--version", action="store", type="string", 
                      dest="version", default='4.0',
                      help="version of WingIDE")
    (options, args) = parser.parse_args(sys.argv[1:])
    
    wingpath = options.wingpath
    projpath = options.projpath
    version = options.version
    if len(version)==1:
        version = version + '.0'
    
    if not os.path.isfile(projpath):
        venvdir = os.path.dirname(os.path.dirname(sys.executable))
        proj_template = os.path.join(os.path.dirname(venvdir),
                                     'config','wing_proj_template.wpr')
        projpath = os.path.join(venvdir, 'etc', 'wingproj.wpr')
        _modify_wpr_file(proj_template, projpath, version)
        
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
        cmd = ['open', projpath]
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
