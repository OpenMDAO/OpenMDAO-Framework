"""
Generates a virtualenv bootstrapping script that will create a 
virtualenv with openmdao and all of its dependencies installed in it.
The script is written to a file called go-openmdao-<version>.py.
"""

import sys, os
from optparse import OptionParser
import virtualenv
from pkg_resources import working_set, Requirement



def main():
    
    script_str = """

def adjust_options(options, args):
    if sys.version_info[:2] < (2,6) or sys.version_info[:2] >= (3,0):
        print 'ERROR: python version must be >= 2.6 and <= 3.0. yours is %%s' %% sys.version.split(' ')[0]
        sys.exit(-1)
    # using distribute requires elevation on Vista, not sure why because setuptools doesn't...
    #options.use_distribute = True  # force use of distribute instead of setuptools
    # name of virtualenv defaults to openmdao-<version>
    if len(args) == 0:
        args.append('openmdao-%%s' %% '%(version)s')
    
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
    subprocess.check_call(cmdline)

def after_install(options, home_dir):
    global logger
    reqs = %(reqs)s
    cmds = %(cmds)s
    url = '%(url)s'
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
    try:
        for req in reqs:
            _single_install(cmds, req, bin_dir)
    except Exception as err:
        print "ERROR: build failed"
        sys.exit(-1)
            
    print '\\n\\nThe OpenMDAO virtual environment has been installed in %%s.' %% home_dir
    print 'From %%s, type:\\n' %% home_dir
    if sys.platform == 'win32':
        print r'Scripts\\activate'
    else:
        print '. bin/activate'
    print "\\nto activate your environment and start using OpenMDAO."

    """
    parser = OptionParser()
    parser.add_option("-d", "--destination", action="store", type="string", dest='dest', 
                      help="specify destination directory", default='.')
    parser.add_option("-t", "--test", action="store_true", dest="test",
                      help="if present, generated installer will point to /OpenMDAO/test_server/dists")
    
    
    (options, args) = parser.parse_args()
    
    openmdao_pkgs = ['openmdao.util', 
                     'openmdao.units', 
                     'openmdao.main', 
                     'openmdao.lib', 
                     'openmdao.test', 
                     'openmdao.examples.simple',
                     'openmdao.examples.bar3simulation',
                     'openmdao.examples.mdao',
                     'openmdao.examples.enginedesign',
                     'openmdao.examples.singleEI',
                    ]

    cmds = []
    reqs = set()
    import openmdao.main.releaseinfo
    version = openmdao.main.releaseinfo.__version__
    dists = working_set.resolve([Requirement.parse(r) for r in openmdao_pkgs])
    excludes = set(['setuptools', 'distribute', 'numpy', 'scipy'])
    for dist in dists:
        if dist.project_name not in excludes:
            reqs.add('%s' % dist.as_requirement())  
            
    if options.test:
        home = os.environ['HOME']
        url = 'file://%s/dists' % home
    else:
        url = 'http://openmdao.org/dists'
    reqs = ['numpy', 'scipy'] + list(reqs)  # force numpy to be installed first (f2py requires it)
    optdict = { 'reqs': reqs, 'cmds':cmds, 'version': version, 'url': url }
    
    dest = os.path.abspath(options.dest)
    scriptname = os.path.join(dest,'go-openmdao-%s.py' % version)
    with open(scriptname, 'wb') as f:
        f.write(virtualenv.create_bootstrap_script(script_str % optdict))
    os.chmod(scriptname, 0755)

if __name__ == '__main__':
    main()
