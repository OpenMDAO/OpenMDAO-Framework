"""
Generates a virtualenv bootstrapping script that will create a virtualenv with
develop versions of all of the openmdao packages.
"""

import sys, os
from optparse import OptionParser
import virtualenv

def main():
    
    script_str = """

# list of openmdao packages to be installed as 'develop' eggs.
# NOTE: Order matters here.  Any given package must appear
#       before any other packages that depend on it.
openmdao_packages = ['openmdao.util', 
                     'openmdao.units', 
                     'openmdao.main', 
                     'openmdao.lib', 
                     'openmdao.test', 
                     'examples/openmdao.examples.simple',
                     'examples/openmdao.examples.bar3simulation',
                     'examples/openmdao.examples.enginedesign',
                    ]

def adjust_options(options, args):
    if sys.version_info[:2] < (2,6) or sys.version_info[:2] >= (3,0):
        print 'ERROR: python version must be >= 2.6 and <= 3.0. yours is %%s' %% sys.version.split(' ')[0]
        sys.exit(-1)
    
def _single_install(cmds, req, bin_dir):
    cmdline = [join(bin_dir, 'easy_install')] + cmds + [req]
    logger.debug("running command: %%s" %% ' '.join(cmdline))
    subprocess.check_call(cmdline)

def _find_repo_top():
    start = os.getcwd()
    location = os.getcwd()
    while location:
        if '.bzr' in os.listdir(location):
            return location
        location = os.path.dirname(location)
    raise RuntimeError('ERROR: %%s is not inside of a bazaar repository' %% start)
    
def after_install(options, home_dir):
    global logger
    reqs = %(reqs)s
    cmds = %(cmds)s
    if not cmds:
        cmds = ['-f','http://openmdao.org/dists']
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
    reqnumpy = 'numpy'   # TODO: grab openmdao dist and query its deps for specific numpy version
    _single_install(cmds, reqnumpy, bin_dir) # force numpy first so we can use f2py later
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
    """
    parser = OptionParser()
    # setuptools doesn't seem to support multiple find-links, but pip does
    parser.add_option("-f", "--find-links", action="append", type="string", dest='flinks', 
                      help="find-links URL") 
    parser.add_option("-r", "--requirement", action="append", type="string", dest='reqs', 
                      help="add an additional required package (multiple are allowed)")
    
    (options, args) = parser.parse_args()
    
    reqs = options.reqs if options.reqs is not None else []
    if options.flinks is not None:
        cmds = [ '-f %s' % x for x in options.flinks]
    else:
        cmds = []
    
    optdict = { 'reqs': reqs, 'cmds':cmds }
    
    with open('go-openmdao-dev.py', 'wb') as f:
        f.write(virtualenv.create_bootstrap_script(script_str % optdict))
    os.chmod('go-openmdao-dev.py', 0755)

if __name__ == '__main__':
    main()
