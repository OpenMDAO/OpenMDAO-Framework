"""
Generates a virtualenv bootstrapping script called go-openmdao-dev.py that will create a 
virtualenv with 'develop' versions of all of the openmdao packages.
"""

import sys, os
from optparse import OptionParser
import virtualenv
from pkg_resources import working_set, Requirement

def main():
    
    script_str = """

# list of openmdao packages to be installed as 'develop' eggs.
openmdao_packages = ['openmdao.util', 
                     'openmdao.units', 
                     'openmdao.main', 
                     'openmdao.lib', 
                     'openmdao.test',
                     'openmdao.devtools',
                     'examples/openmdao.examples.simple',
                     'examples/openmdao.examples.bar3simulation',
                     'examples/openmdao.examples.enginedesign',
                    ]

def _find_repo_top():
    start = os.getcwd()
    location = os.getcwd()
    while location:
        if '.bzr' in os.listdir(location):
            return location
        tmp = location
        location = os.path.dirname(location)
        if tmp == location:
            break
    raise RuntimeError('ERROR: %%s is not inside of a bazaar repository' %% start)
    
def adjust_options(options, args):
    if sys.version_info[:2] < (2,6) or sys.version_info[:2] >= (3,0):
        print 'ERROR: python version must be >= 2.6 and <= 3.0. yours is %%s' %% sys.version.split(' ')[0]
        sys.exit(-1)
    for arg in args:
        if not arg.startswith('-'):
            print 'no args allowed that start without a dash (-)'
            sys.exit(-1)
    args.append(join(_find_repo_top(), 'devenv'))  # force the virtualenv to be in <repo_top>/devenv

def _single_install(cmds, req, bin_dir):
    #import pkg_resources
    #try:
        #pkg_resources.working_set.resolve([pkg_resources.Requirement.parse(req)])
    #except (pkg_resources.DistributionNotFound, pkg_resources.VersionConflict):
        ## if package isn't currently installed, install it
        ## NOTE: we need to do our own check for already installed packages because
        ##       for some reason distribute always wants to install a package even if
        ##       it already is installed.
    cmdline = [join(bin_dir, 'easy_install'),'-NZ'] + cmds + [req]
        # pip seems more robust than easy_install, but won't install from binary distribs :(
        #cmdline = [join(bin_dir, 'pip'), 'install'] + cmds + [req]
    logger.debug("running command: %%s" %% ' '.join(cmdline))
    subprocess.call(cmdline)

def after_install(options, home_dir):
    global logger
    reqs = %(reqs)s
    cmds = %(cmds)s
    url = 'http://openmdao.org/dists'
    found = [c for c in cmds if url in c]
    if not found:
        cmds.extend(['-f',url])
    etc = join(home_dir, 'etc')
    ## TODO: this should all come from distutils
    ## like distutils.sysconfig.get_python_inc()
    if sys.platform == 'win32':
        lib_dir = join(home_dir, 'Lib')
        bin_dir = join(home_dir, 'Scripts')
    elif is_jython:
        lib_dir = join(home_dir, 'Lib')
        bin_dir = join(home_dir, 'bin')
    else:
        lib_dir = join(home_dir, 'lib', py_version)
        bin_dir = join(home_dir, 'bin')

    if not os.path.exists(etc):
        os.makedirs(etc)
    reqnumpy = 'numpy'
    numpyidx = None
    for i,req in enumerate(reqs):
        if req.startswith('numpy') and len(req)>5 and (req[5]=='=' or req[5]=='>'):
            # for now, just require 'numpy' instead of a specific version
            #reqnumpy = req
            numpyidx = i
            break
    _single_install(cmds, reqnumpy, bin_dir) # force numpy first so we can use f2py later
    if numpyidx is not None:
        reqs.remove(reqs[numpyidx])
    for req in reqs:
        _single_install(cmds, req, bin_dir)

    # now install dev eggs for all of the openmdao packages
    topdir = _find_repo_top()
    startdir = os.getcwd()
    absbin = os.path.abspath(bin_dir)
    try:
        for pkg in openmdao_packages:
            os.chdir(join(topdir, pkg))
            cmdline = [join(absbin, 'python'), 'setup.py', 'develop'] + cmds
            subprocess.check_call(cmdline)
    finally:
        os.chdir(startdir)
        
    # copy the default wing project file into the virtualenv
    # try to find the default.wpr file in the user's home directory
    try:
        if sys.platform == 'win32':
            home = os.environ['HOMEDRIVE']+os.environ['HOMEPATH']
        else:
            home = os.environ['HOME']
    except:
        home = ''
    
    proj_template = join(home, '.wingide3', 'default.wpr')
    if not os.path.isfile(proj_template):
        proj_template = join(topdir,'config','wing_proj_template.wpr')
    
    shutil.copy(proj_template, 
                join(os.path.abspath(home_dir),'etc','wingproj.wpr'))
                
    print '\\n\\nThe OpenMDAO virtual environment has been installed in %%s.' %% home_dir
    print 'From %%s, type:\\n' %% home_dir
    if sys.platform == 'win32':
        print r'Scripts\\activate'
    else:
        print '. bin/activate'
    print "\\nto activate your environment and start using OpenMDAO."
    """
    parser = OptionParser()
    
    (options, args) = parser.parse_args()
    
    openmdao_pkgs = ['openmdao.util', 
                     'openmdao.units', 
                     'openmdao.main', 
                     'openmdao.lib', 
                     'openmdao.test', 
                     'openmdao.devtools',
                     'openmdao.examples.simple',
                     'openmdao.examples.bar3simulation',
                     'openmdao.examples.enginedesign',
                    ]

    cmds = []
    reqs = []
    dists = working_set.resolve([Requirement.parse(r) for r in openmdao_pkgs])
    excludes = set(['setuptools', 'distribute'])
    for dist in dists:
        if dist.project_name == 'openmdao.main':
            version = dist.version
        if not dist.project_name.startswith('openmdao.') and dist.project_name not in excludes:
            reqs.append('%s' % dist.as_requirement())  
            
    reqs = list(set(reqs))  # eliminate duplicates (numpy was in there twice somehow)
    
    optdict = { 'reqs': reqs, 'cmds':cmds }
    
    with open('go-openmdao-dev.py', 'wb') as f:
        f.write(virtualenv.create_bootstrap_script(script_str % optdict))
    os.chmod('go-openmdao-dev.py', 0755)

if __name__ == '__main__':
    main()
