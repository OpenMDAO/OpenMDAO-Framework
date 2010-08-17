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
                     'examples/openmdao.examples.mdao',
                     'examples/openmdao.examples.singleEI'
                    ]
    
def adjust_options(options, args):
    if sys.version_info[:2] < (2,6) or sys.version_info[:2] >= (3,0):
        print 'ERROR: python version must be >= 2.6 and <= 3.0. yours is %%s' %% sys.version.split(' ')[0]
        sys.exit(-1)
    for arg in args:
        if not arg.startswith('-'):
            print 'no args allowed that start without a dash (-)'
            sys.exit(-1)
    args.append(join(os.path.dirname(__file__), 'devenv'))  # force the virtualenv to be in <top>/devenv

def _single_install(cmds, req, bin_dir):
    cmdline = [join(bin_dir, 'easy_install'),'-NZ'] + cmds + [req]
        # pip seems more robust than easy_install, but won't install from binary distribs :(
        #cmdline = [join(bin_dir, 'pip'), 'install'] + cmds + [req]
    logger.debug("running command: %%s" %% ' '.join(cmdline))
    subprocess.check_call(cmdline)

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
    try:
        for req in reqs:
            _single_install(cmds, req, bin_dir)

        # now install dev eggs for all of the openmdao packages
        topdir = os.path.abspath(os.path.dirname(__file__))
        startdir = os.getcwd()
        absbin = os.path.abspath(bin_dir)
        try:
            for pkg in openmdao_packages:
                os.chdir(join(topdir, pkg))
                cmdline = [join(absbin, 'python'), 'setup.py', 
                           'develop', '-N'] + cmds
                subprocess.check_call(cmdline)
        finally:
            os.chdir(startdir)
    except Exception as err:
        print "ERROR: build failed"
        sys.exit(-1)
        
    # copy the wing project file into the virtualenv
    proj_template = join(topdir,'config','wing_proj_template.wpr')
    
    abshome = os.path.abspath(home_dir)
    shutil.copy(proj_template, 
                join(abshome,'etc','wingproj.wpr'))
                
    print '\\n\\nThe OpenMDAO virtual environment has been installed in %%s.' %% abshome
    print 'From %%s, type:\\n' %% abshome
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
                     'openmdao.examples.mdao',
                     'openmdao.examples.singleEI'
                    ]

    cmds = []
    reqs = set()
    dists = working_set.resolve([Requirement.parse(r) for r in openmdao_pkgs])
    excludes = set(['setuptools', 'distribute', 'numpy', 'scipy'])
    for dist in dists:
        if dist.project_name == 'openmdao.main':
            version = dist.version
        if not dist.project_name.startswith('openmdao.') and dist.project_name not in excludes:
            reqs.add('%s' % dist.as_requirement())  
            
    reqs = ['numpy', 'scipy'] + list(reqs)
    
    optdict = { 'reqs': reqs, 'cmds':cmds }
    
    with open('go-openmdao-dev.py', 'wb') as f:
        f.write(virtualenv.create_bootstrap_script(script_str % optdict))
    os.chmod('go-openmdao-dev.py', 0755)

if __name__ == '__main__':
    main()
