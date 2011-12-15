"""
Generates either a go-openmdao.py script for installation
of an openmdao release or a go-openmdao-dev.py script for creating a 
virtualenv with 'develop' versions of all of the openmdao packages. Both
scripts bootstrap a virtualenv environment.

Use the --dev option to generate a go-openmdao-dev.py script.

This script should be run from within an activated OpenMDAO virtual environment
in order to capture all of the required dependencies correctly.
"""

import sys, os
import virtualenv
import pprint
import StringIO
import shutil
from pkg_resources import working_set, Requirement
from optparse import OptionParser

#
#      EDIT THE FOLLOWING TWO LISTS TO CONTROL THE PACKAGES THAT WILL BE
#      REQUIRED FOR THE OPENMDAO RELEASE AND DEVELOPMENT ENVIRONMENTS
#
# List of packages that must be pre-installed before openmdao can be installed.
# The presence of these will be checked at the beginning and the install will abort
# if they're not found, because otherwise easy_install will try to build them
# and will fail with massive output of stuff that may be confusing to the user.
# If the user runs the installer with a --noprereqs arg, then the prerequisites
# won't be checked.
openmdao_prereqs = ['numpy', 'scipy']

# list of tuples of the form: (openmdao package, parent directory relative to
# the top of the repository, package type). The directory only has meaning for
# a dev install and is ignored in the user install. '' should be used instead
# of '.' to indicate that the parent dir is the top of the repository. The
# package type should be 'sdist' or 'bdist_egg' for binary packages
openmdao_packages = [('openmdao.util', '', 'sdist'), 
                     ('openmdao.units', '', 'sdist'), 
                     ('openmdao.main', '', 'sdist'), 
                     ('openmdao.lib', '', 'sdist'), 
                     ('openmdao.test', '', 'sdist'),
                     ('openmdao.gui', '', 'sdist'),
                     ('openmdao.examples.simple', 'examples', 'sdist'),
                     ('openmdao.examples.bar3simulation', 'examples', 'bdist_egg'),
                     ('openmdao.examples.enginedesign', 'examples', 'bdist_egg'),
                     ('openmdao.examples.mdao', 'examples', 'sdist'),
                     ('openmdao.examples.expected_improvement', 'examples', 'sdist'),
                    ]

# if it's a dev installer, these packages will also be included
openmdao_dev_packages = [('openmdao.devtools', '', 'sdist'),
                         ]

def _get_adjust_options(options, version):
    """Return a string containing the definition of the adjust_options function
    that will be included in the generated virtualenv bootstrapping script.
    """
    if options.dev:
        code = """
    for arg in args:
        if not arg.startswith('-'):
            print 'ERROR: no args allowed that start without a dash (-)'
            sys.exit(-1)
    args.append(join(os.path.dirname(__file__), 'devenv'))  # force the virtualenv to be in <top>/devenv
"""
    else:
        code = """
    # name of virtualenv defaults to openmdao-<version>
    if len(args) == 0:
        args.append('openmdao-%%s' %% '%s')
""" % version
    
    return """
def adjust_options(options, args):
    major_version = sys.version_info[:2]
    if major_version < (2,6) or major_version > (3,0):
        print 'ERROR: python major version must be 2.6 or 2.7. yours is %%s' %% str(major_version)
        sys.exit(-1)
%s

""" % code

def main(args=None):
    if args is None:
        args = sys.argv[1:]

    parser = OptionParser()
    parser.add_option("--dev", action="store_true", dest='dev', 
                      help="if present, a development script will be generated instead of a release script")
    parser.add_option("--dest", action="store", type="string", dest='dest', 
                      help="specify destination directory", default='.')
    parser.add_option("-f", "--findlinks", action="store", type="string", 
                      dest="findlinks",
                      default='http://openmdao.org/dists',
                      help="default URL where openmdao packages and dependencies are searched for first (before PyPI)")
    
    (options, args) = parser.parse_args(args)
    
    if len(args) > 0:
        print 'unrecognized args: %s' % args
        parser.print_help()
        sys.exit(-1)

    if options.dev:
        openmdao_packages.extend(openmdao_dev_packages)
        sout = StringIO.StringIO()
        pprint.pprint(openmdao_packages, sout)
        pkgstr = sout.getvalue()
        make_dev_eggs = """
        # now install dev eggs for all of the openmdao packages
        topdir = os.path.abspath(os.path.dirname(__file__))
        startdir = os.getcwd()
        absbin = os.path.abspath(bin_dir)
        openmdao_packages = %s
        try:
            for pkg, pdir, _ in openmdao_packages:
                if not options.gui and pkg == 'openmdao.gui':
                    continue
                os.chdir(join(topdir, pdir, pkg))
                cmdline = [join(absbin, 'python'), 'setup.py', 
                           'develop', '-N'] + cmds
                try:
                    call_subprocess(cmdline, show_stdout=True, raise_on_returncode=True)
                except OSError:
                    failures.append(pkg)
        finally:
            os.chdir(startdir)
        """ % pkgstr
    else:  # making a release installer
        make_dev_eggs = ''

    script_str = """

openmdao_prereqs = %(openmdao_prereqs)s

def extend_parser(parser):
    parser.add_option("-r","--req", action="append", type="string", dest='reqs', 
                      help="specify additional required distributions", default=[])
    parser.add_option("--noprereqs", action="store_true", dest='noprereqs', 
                      help="don't check for any prerequisites, e.g., numpy or scipy")
    parser.add_option("--gui", action="store_true", dest='gui', 
                      help="install the openmdao graphical user interface and its dependencies")
    parser.add_option("-f", "--findlinks", action="store", type="string", 
                      dest="findlinks",
                      help="default URL where openmdao packages and dependencies are searched for first (before PyPI)")
    parser.add_option("--testurl", action="store", type="string", dest='testurl', 
                      help="specify url where openmdao.* distribs are located (used for release testing only)")
                      
    # hack to force use of setuptools for now because using 'distribute' causes issues
    os.environ['VIRTUALENV_USE_SETUPTOOLS'] = '1'

%(adjust_options)s

def _single_install(cmds, req, bin_dir, failures, dodeps=False):
    global logger
    if dodeps:
        extarg = '-Z'
    else:
        extarg = '-NZ'
    cmdline = [join(bin_dir, 'easy_install'), extarg] + cmds + [req]
        # pip seems more robust than easy_install, but won't install binary distribs :(
        #cmdline = [join(bin_dir, 'pip'), 'install'] + cmds + [req]
    #logger.debug("running command: %%s" %% ' '.join(cmdline))
    try:
        call_subprocess(cmdline, show_stdout=True, raise_on_returncode=True)
    except OSError:
        failures.append(req)

def after_install(options, home_dir):
    global logger, openmdao_prereqs
    
    reqs = %(reqs)s
    guireqs = %(guireqs)s
    
    if options.findlinks is None:
        url = '%(url)s'
    else:
        url = options.findlinks
    # for testing we allow one to specify a url where the openmdao
    # package dists are located that may be different from the main
    # url where the dependencies are located. We do this because
    # setuptools only allows us to specify a single -f parameter,
    # which would force us to mirror the entire openmdao distribution
    # directory in order to test our releases because setuptools will
    # barf if it can't find everything in the same location (or on PyPI).
    # TODO: get rid of this after we quit using setuptools.
    if options.testurl:
        openmdao_url = options.testurl
    else:
        openmdao_url = url
    etc = join(home_dir, 'etc')
    if sys.platform == 'win32':
        lib_dir = join(home_dir, 'Lib')
        bin_dir = join(home_dir, 'Scripts')
    else:
        lib_dir = join(home_dir, 'lib', py_version)
        bin_dir = join(home_dir, 'bin')

    if not os.path.exists(etc):
        os.makedirs(etc)
        
    failed_imports = []
    for pkg in openmdao_prereqs:
        try:
            __import__(pkg)
        except ImportError:
            failed_imports.append(pkg)
    if failed_imports:
        if options.noprereqs:
            logger.warn("\\n**** The following prerequisites could not be imported: %%s." %% failed_imports)
            logger.warn("**** As a result, some OpenMDAO components will not work.")
        else:
            logger.error("ERROR: the following prerequisites could not be imported: %%s." %% failed_imports)
            logger.error("These must be installed in the system level python before installing OpenMDAO.")
            logger.error("To run a limited version of OpenMDAO without the prerequisites, try 'python %%s --noprereqs'" %% __file__)
            sys.exit(-1)
    
    cmds = ['-f', url]
    openmdao_cmds = ['-f', openmdao_url]
    try:
        allreqs = reqs[:]
        failures = []
        if options.gui:
            allreqs = allreqs + guireqs
            
        for req in allreqs:
            if req.startswith('openmdao.'):
                _single_install(openmdao_cmds, req, bin_dir, failures)
            else:
                _single_install(cmds, req, bin_dir, failures)
        
%(make_dev_eggs)s

        # add any additional packages specified on the command line
        for req in options.reqs:
            _single_install(cmds, req, bin_dir, failures, dodeps=True)

    except Exception as err:
        logger.error("ERROR: build failed: %%s" %% str(err))
        sys.exit(-1)

    abshome = os.path.abspath(home_dir)
    
    if failures:
        failmsg = ' (with failures).'
        failures.sort()
        print '\\n\\n***** The following packages failed to install: %%s.' %% failures
    else:
        failmsg = '.'
    print '\\n\\nThe OpenMDAO virtual environment has been installed in\\n %%s%%s' %% (abshome, failmsg)
    print '\\nFrom %%s, type:\\n' %% abshome
    if sys.platform == 'win32':
        print r'Scripts\\activate'
    else:
        print '. bin/activate'
    print "\\nto activate your environment and start using OpenMDAO."
    
    sys.exit(1 if failures else 0)
    """
    
    reqs = set()
    guireqs = set()
    
    version = '?.?.?'
    excludes = set(['setuptools', 'distribute', 'SetupDocs']+openmdao_prereqs)
    dists = working_set.resolve([Requirement.parse(r[0]) 
                                   for r in openmdao_packages if r[0]!='openmdao.gui'])
    distnames = set([d.project_name for d in dists])-excludes
    gui_dists = working_set.resolve([Requirement.parse('openmdao.gui')])
    guinames = set([d.project_name for d in gui_dists])-distnames-excludes
    
    try:
        setupdoc_dist = working_set.resolve([Requirement.parse('setupdocs')])[0]
    except:
        setupdoc_dist = None
        
    for dist in dists:
        if dist.project_name not in distnames:
            continue
        if dist.project_name == 'openmdao.main':
            version = dist.version
        if options.dev: # in a dev build, exclude openmdao stuff because we'll make them 'develop' eggs
            if not dist.project_name.startswith('openmdao.'):
                reqs.add('%s' % dist.as_requirement())
        else:
            reqs.add('%s' % dist.as_requirement())
            
    for dist in gui_dists:
        if dist.project_name not in guinames:
            continue
        if options.dev: # in a dev build, exclude openmdao stuff because we'll make them 'develop' eggs
            if not dist.project_name.startswith('openmdao.'):
                guireqs.add('%s' % dist.as_requirement())
        else:
            guireqs.add('%s' % dist.as_requirement())

    # adding setupdocs req is a workaround to prevent Traits from looking elsewhere for it
    if setupdoc_dist:
        _reqs = [str(setupdoc_dist.as_requirement())]
    else:
        _reqs = ['setupdocs>=1.0']
    reqs = _reqs + list(reqs) 
    guireqs = list(guireqs)
    
    optdict = { 
        'reqs': reqs, 
        'guireqs': guireqs,
        'version': version, 
        'url': options.findlinks,
        'make_dev_eggs': make_dev_eggs,
        'adjust_options': _get_adjust_options(options, version),
        'openmdao_prereqs': openmdao_prereqs,
    }
    
    dest = os.path.abspath(options.dest)
    if options.dev:
        scriptname = os.path.join(dest,'go-openmdao-dev.py')
    else:
        scriptname = os.path.join(dest,'go-openmdao-%s.py' % version)
    if os.path.isfile(scriptname):
        shutil.copyfile(scriptname, scriptname+'.old'
                        )
    with open(scriptname, 'wb') as f:
        f.write(virtualenv.create_bootstrap_script(script_str % optdict))
    os.chmod(scriptname, 0755)
    
if __name__ == '__main__':
    main()
