"""
Generates either a ``go-openmdao-<version>.py`` script for installation
of an openmdao release or ``a go-openmdao-dev.py`` script for creating a
virtualenv with "develop" versions of all of the openmdao packages. Both
scripts bootstrap a virtualenv environment.

Use the --dev option to generate a ``go-openmdao-dev.py`` script.

This script should be run from within an activated OpenMDAO virtual environment
in order to capture all of the required dependencies correctly.
"""

import sys
import os
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
openmdao_packages = [
    ('openmdao.util',  '', 'sdist'),
    ('openmdao.units', '', 'sdist'),
    ('openmdao.main',  '', 'sdist'),
    ('openmdao.lib',   '', 'sdist'),
    ('openmdao.test',  '', 'sdist'),
    ('openmdao.examples.simple',               'examples', 'sdist'),
    ('openmdao.examples.bar3simulation',       'examples', 'bdist_egg'),
    ('openmdao.examples.mdao',                 'examples', 'sdist'),
    ('openmdao.examples.metamodel_tutorial',   'examples', 'sdist'),
    ('openmdao.examples.expected_improvement', 'examples', 'sdist'),
    ('openmdao.examples.nozzle_geometry_doe',  'examples', 'sdist')
]


# if it's a dev installer, these packages will also be included
openmdao_dev_packages = [
    ('openmdao.devtools', '', 'sdist'),
]


def _get_adjust_options(options, version, setuptools_url, setuptools_version):
    """Return a string containing the definition of the adjust_options function
    that will be included in the generated virtualenv bootstrapping script.
    """
    anaconda_error = None

    if options.dev:
        code = """
    for arg in args:
        if not arg.startswith('-'):
            print 'ERROR: no args allowed that start without a dash (-)'
            sys.exit(-1)
    args.append(join(os.path.dirname(__file__), 'devenv'))  # force the virtualenv to be in <top>/devenv
"""
        anaconda_error = "print 'ERROR: OpenMDAO go scripts cannot be used with Anaconda distributions.\\nFor more instructions on installing the dev version run:\\n\\n\\t python openmdao.devtools/src/openmdao/devtools/conda_build.py dev --help'"

    else:
        code = """
    # name of virtualenv defaults to openmdao-<version>
    if len(args) == 0:
        args.append('openmdao-%%s' %% '%s')
""" % version

        anaconda_error = "print 'ERROR: OpenMDAO go scripts cannot be used with Anaconda distributions.\\nUse the command below to install the latest version of OpenMDAO:\\n\\n\\tconda create --name <environment name> openmdao'"

    adjuster = """
def adjust_options(options, args):
    version = sys.version

    if "Analytics" in version or "Anaconda" in version:
        %s
        sys.exit(-1)

    major_version = sys.version_info[:2]
    if major_version != (2,7):
        print 'ERROR: python major version must be 2.7, yours is %%s' %% str(major_version)
        sys.exit(-1)
%s
    # Check if we're running in an activated environment.
    virtual_env = os.environ.get('VIRTUAL_ENV')

    if options.relocatable:
        import distutils.util
        import zipfile

        if not virtual_env:
            print 'ERROR: --relocatable requires an activated environment'
            sys.exit(-1)

        # Make current environment relocatable.
        make_environment_relocatable(virtual_env)

        # Copy files to archive.
        base = os.path.basename(virtual_env)
        zipname = '%%s-%%s.zip' %% (base, distutils.util.get_platform())
        print 'Packing the relocatable environment into', zipname
        count = 0
        with zipfile.ZipFile(zipname, 'w', zipfile.ZIP_DEFLATED) as zipped:
            for dirpath, dirname, filenames in os.walk(virtual_env):
                arcpath = os.path.join(base, dirpath[len(virtual_env)+1:])
                for filename in filenames:
                    count += 1
                    if (count %% 100) == 0:
                        sys.stdout.write('.')
                        sys.stdout.flush()
                    zipped.write(os.path.join(dirpath, filename),
                                 os.path.join(arcpath, filename))
            zipped.writestr(os.path.join(base, 'script-fixer.py'),
                            _SCRIPT_FIXER)
        print "\\nRemember to run 'python script-fixer.py' after unpacking."
        sys.exit(0)

    if virtual_env:
        # Install in current environment.
        after_install(options, virtual_env, activated=True)

    try:
        if not is_win:
            download('%s')
        import ez_setup
        ez_setup.use_setuptools(version='%s', download_base='https://openmdao.org/dists')
        os.remove('ez_setup.py')
    except Exception as err:
        logger.warn(str(err))

""" % (anaconda_error, code, setuptools_url, setuptools_version)

    fixer = '''
_SCRIPT_FIXER = """\\
import glob
import os.path
import sys


def main():
    # Move to script directory of the unzipped environment.
    root = os.path.dirname(os.path.abspath(__file__))
    scripts = 'Scripts' if sys.platform == 'win32' else 'bin'
    scripts = os.path.join(root, scripts)
    os.chdir(scripts)
    tmpname = 'script-to-fix'

    # Fix activate scripts.
    for filename in sorted(glob.glob('activate*')):
        if filename == 'activate':         # Bourne/bash.
            pattern = 'VIRTUAL_ENV="'
        elif filename == 'activate.csh':   # C shell.
            pattern = 'setenv VIRTUAL_ENV "'
        elif filename == 'activate.fish':  # ?
            pattern = 'set -gx VIRTUAL_ENV "'
        elif filename == 'activate.bat':   # Windows.
            pattern = 'set "VIRTUAL_ENV='
        else:
            continue

        print 'Fixing', filename
        if os.path.exists(tmpname):
            os.remove(tmpname)
        os.rename(filename, tmpname)
        with open(tmpname, 'rU') as inp:
            with open(filename, 'w') as out:
                for line in inp:
                    if line.startswith(pattern):
                        line = '%s%s"\\\\n' % (pattern, root)
                    out.write(line)
        os.remove(tmpname)

    # Fix Windows 'shadow' scripts.
    if sys.platform == 'win32':
        replacement = '#!%s\\\\\\\\python.exe\\\\n' % scripts
        for filename in sorted(glob.glob('*-script.py')):
            print 'Fixing', filename
            if os.path.exists(tmpname):
                os.remove(tmpname)
            os.rename(filename, tmpname)
            with open(tmpname, 'rU') as inp:
                with open(filename, 'w') as out:
                   for line in inp:
                       if line.startswith('#!'):
                           line = replacement
                       out.write(line)
            os.remove(tmpname)


if __name__ == '__main__':
    main()
"""'''
    return adjuster + fixer


def main(args=None):
    if args is None:
        args = sys.argv[1:]

    parser = OptionParser()
    parser.add_option("--dev", action="store_true", dest='dev',
                      help="if present, a development script will be generated instead of a release script")
    parser.add_option("--dest", action="store", type="string", dest='dest',
                      help="specify destination directory", default='.')
    parser.add_option("--offline", action="store", type="string", dest='offline',
                      help="make offline gathering script", default='')
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
            if is_darwin:
                extra_env={'ARCHFLAGS': '-Wno-error=unused-command-line-argument-hard-error-in-future'}

            for pkg, pdir, _ in openmdao_packages:
                os.chdir(join(topdir, pdir, pkg))
                cmdline = [join(absbin, 'python'), 'setup.py',
                           'develop', '-N'] + cmds
                try:
                    if is_darwin:
                        call_subprocess(cmdline, show_stdout=True, raise_on_returncode=True, extra_env=extra_env)
                    else:
                        call_subprocess(cmdline, show_stdout=True, raise_on_returncode=True)
                except OSError:
                    failures.append(pkg)
        finally:
            os.chdir(startdir)
        """ % pkgstr
        make_docs = """
        if options.docs:
            if(os.system('%s %s && %s %s build_docs && deactivate'
                         % (source_command, activate, python, openmdao)) != 0):
                print "Failed to build the docs."
        else:
            print "\\nSkipping build of OpenMDAO docs.\\n"
        """
    else:  # making a release installer
        make_dev_eggs = ''
        make_docs = ''

    if options.offline == "gather":
        offline_ = ["'-zmaxd'", "'-zmaxd'", ", 'pkg'", "os.mkdir('pkg')", "['-f', url]", "['-f', openmdao_url]"]
        f_prefix = "gather-"
    elif options.offline == "installer":
        offline_ = ["'-Z'", "'-NZ'", '', '', "['-H', 'None', '-f', options.findlinks]", "['-H', 'None', '-f', options.findlinks]"]
        f_prefix = "offline-"
    else:
        offline_ = ["'-Z'", "'-NZ'", '', '', "['-f', url]", "['-f', openmdao_url]"]
        f_prefix = ""

    script_str = """

openmdao_prereqs = %(openmdao_prereqs)s

%(mkdir_pkg)s

def extend_parser(parser):
    parser.add_option("-r","--req", action="append", type="string", dest='reqs',
                      help="specify additional required distributions", default=[])
    parser.add_option("--noprereqs", action="store_true", dest='noprereqs',
                      help="don't check for any prerequisites, e.g., numpy or scipy")
    parser.add_option("--nodocs", action="store_false", dest='docs', default=True,
                      help="do not build the docs")
    parser.add_option("-f", "--findlinks", action="store", type="string",
                      dest="findlinks",
                      help="default URL where openmdao packages and dependencies are searched for first (before PyPI)")
    parser.add_option("--testurl", action="store", type="string", dest='testurl',
                      help="specify url where openmdao.* distribs are located (used for release testing only)")

    # go back to old behavior that includes system site packages by default
    parser.set_defaults(system_site_packages=True)

%(adjust_options)s


def download(url, dest='.'):
    import urllib2
    dest = os.path.abspath(os.path.expanduser(os.path.expandvars(dest)))

    resp = urllib2.urlopen(url)
    outpath = os.path.join(dest, os.path.basename(url))
    bs = 1024*8
    with open(outpath, 'wb') as out:
        while True:
            block = resp.fp.read(bs)
            if block == '':
                break
            out.write(block)
    return outpath

def _get_mingw_dlls(bin_dir):
    def _mingw_dlls_in_path():
        # first, check if MinGW/bin is already in PATH
        if 'path' in os.environ:
            for entry in os.environ['PATH'].split(os.pathsep):
                if os.path.isfile(os.path.join(entry, 'libgfortran-3.dll')):
                    print 'MinGW is already installed, skipping download.'
                    return True

        return False

    def _get_mingw_dlls_from_site(bin_dir):
        import zipfile
        dest = os.path.abspath(bin_dir)
        zippath = download('http://openmdao.org/releases/misc/mingwdlls.zip')
        zipped = zipfile.ZipFile(zippath, 'r')
        zipped.extractall(dest)
        zipped.close()
        os.remove(zippath)

    _mingw_dlls_in_path() or _get_mingw_dlls_from_site(bin_dir)

def _single_install(cmds, req, bin_dir, failures, dodeps=False):
    global logger
    if dodeps:
        extarg = %(extarg1)s
    else:
        extarg = %(extarg2)s

    #To get rid of OSX 10.9 compiler errors by turning them to warnings.
    if is_darwin:
        extra_env={'ARCHFLAGS': '-Wno-error=unused-command-line-argument-hard-error-in-future'}

    # If there are spaces in the install path, the easy_install script
    # will have an invalid shebang line (Linux/Mac only).
    cmdline = [] if is_win else [join(bin_dir, 'python')]
    cmdline += [join(bin_dir, 'easy_install'), extarg %(dldir)s] + cmds + [req]
        # pip seems more robust than easy_install, but won't install binary distribs :(
        #cmdline = [join(bin_dir, 'pip'), 'install'] + cmds + [req]
    #logger.debug("running command: %%s" %% ' '.join(cmdline))
    try:
        if is_darwin:
            call_subprocess(cmdline, show_stdout=True, raise_on_returncode=True, extra_env=extra_env)
        else:
            call_subprocess(cmdline, show_stdout=True, raise_on_returncode=True)
    except OSError:
        failures.append(req)


def _copy_winlibs(home_dir, activated):
    # On windows, builds using numpy.distutils.Configuration will
    # fail when built in a virtualenv
    # (still broken as of virtualenv 1.9.1, under python 2.7.4)
    # because distutils looks for libpython?.?.lib under sys.prefix/libs.
    # virtualenv does not (at this time) create a libs directory.

    import fnmatch
    libsdir = os.path.join(home_dir, 'libs')
    if not os.path.isdir(libsdir):
        os.mkdir(libsdir)
    if activated:
        with open(os.path.join(home_dir, 'Lib', 'orig-prefix.txt')) as inp:
            prefix = inp.readline().strip()
    else:
        prefix = os.path.dirname(sys.executable)
    sysdir = os.path.join(prefix, 'libs')
    names = os.listdir(sysdir)
    for pat in ['*python*', '*msvc*']:
        for name in fnmatch.filter(names, pat):
            if not os.path.isfile(os.path.join(libsdir, name)):
                shutil.copyfile(os.path.join(sysdir, name),
                                os.path.join(libsdir, name))

def _update_easy_manifest(home_dir):
    # distribute requires elevation when run on 32 bit windows,
    # apparently because Windows assumes that any program with
    # 'install' in the name should require elevation.  The
    # solution is to create a manifest file for easy_install.exe
    # that tells Windows that it doesn't require elevated
    # access.
    template = \"\"\"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <assemblyIdentity version="1.0.0.0"
  processorArchitecture="X86"
  name="easy_install.exe"
  type="win32"/>
  <!-- Identify the application security requirements. -->
  <trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
  <security>
  <requestedPrivileges>
  <requestedExecutionLevel level="asInvoker" uiAccess="false"/>
  </requestedPrivileges>
  </security>
  </trustInfo>
</assembly>
    \"\"\"

    bindir = os.path.join(home_dir, 'Scripts')
    manifest = os.path.join(bindir, 'easy_install.exe.manifest')
    if not os.path.isfile(manifest):
        with open(manifest, 'w') as f:
            f.write(template)
        # 'touch' the easy_install executable
        os.utime(os.path.join(bindir, 'easy_install.exe'), None)

def after_install(options, home_dir, activated=False):
    global logger, openmdao_prereqs

    setuptools_version = "0.9.5"
    setuptools_egg = \"setuptools-%%s-py%%s.egg\" %% (setuptools_version, sys.version[:3])

    if(os.path.exists(setuptools_egg)):
        os.remove(setuptools_egg)

    reqs = %(reqs)s

    if options.findlinks is None:
        url = '%(url)s'
    else:
        url = options.findlinks
    # For testing we allow one to specify a url where the openmdao
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
    if is_win:
        lib_dir = join(home_dir, 'Lib')
        bin_dir = join(home_dir, 'Scripts')
    else:
        lib_dir = join(home_dir, 'lib', py_version)
        bin_dir = join(home_dir, 'bin')

    if not os.path.exists(etc):
        os.makedirs(etc)

    if is_win:
        _copy_winlibs(home_dir, activated)
        _update_easy_manifest(home_dir)
    else:
        # Put lib64_path at front of paths rather than end.
        # As of virtualenv 1.8.2 this fix had not made it in the release.
        patched = False
        site_orig = join(lib_dir, 'site.py')
        site_patched = join(lib_dir, 'site-patched.py')
        with open(site_orig, 'r') as inp:
            with open(site_patched, 'w') as out:
                for line in inp:
                    if 'paths.append(lib64_path)' in line:
                        print 'Patching site.py...'
                        print '  found %%r' %% line
                        line = line.replace('append(', 'insert(0, ')
                        print '    new %%r' %% line
                        sys.stdout.flush()
                        patched = True
                    out.write(line)
        if patched:
            os.rename(site_orig, join(lib_dir, 'site-orig.py'))
            os.rename(site_patched, site_orig)

    failed_imports = []
    for pkg in openmdao_prereqs:
        try:
            __import__(pkg)
        except ImportError:
            failed_imports.append(pkg)

        #Hack to make sure scipy is up to date.
        try:
            from scipy.optimize import minimize
        except:
            if "scipy" in failed_imports:
                failed_imports.remove("scipy")
            failed_imports.append("scipy>=0.11.0")

    if failed_imports:
        if options.noprereqs:
            print "\\n**** The following prerequisites could not be imported: %%s." %% failed_imports
            print "**** As a result, some OpenMDAO components will not work."
        else:
            print "ERROR: the following prerequisites could not be imported: %%s." %% failed_imports
            print "These must be installed in the system level python before installing OpenMDAO."
            print "To run a limited version of OpenMDAO without the prerequisites, try 'python %%s --noprereqs'" %% __file__
            sys.exit(-1)

    cmds = %(cmds_str)s
    openmdao_cmds = %(openmdao_cmds_str)s
    try:
        allreqs = reqs[:]
        failures = []

        for req in allreqs:
            if req.startswith('openmdao.'):
                _single_install(openmdao_cmds, req, bin_dir, failures)
            else:
                _single_install(cmds, req, bin_dir, failures)

%(make_dev_eggs)s

        # add any additional packages specified on the command line
        for req in options.reqs:
            _single_install(cmds, req, bin_dir, failures, dodeps=True)

        activate = os.path.join(bin_dir, 'activate')
        deactivate = os.path.join(bin_dir, 'deactivate')
        if is_win:
            source_command = ''
            python = ''
            openmdao = 'openmdao'
        else:
            source_command = '.'
            python = os.path.join(bin_dir, 'python')
            openmdao = os.path.join(bin_dir, 'openmdao')

%(make_docs)s
        if is_win: # retrieve MinGW DLLs from server
            try:
                _get_mingw_dlls(bin_dir)
            except Exception as err:
                print str(err)
                print "\\n\\n**** Failed to download MinGW DLLs, so OpenMDAO extension packages may fail to load."
                print "If you install MinGW yourself (including c,c++, and fortran compilers) and put "
                print "the MinGW bin directory in your path, that should fix the problem."
    except Exception as err:
        print "ERROR: build failed: %%s" %% str(err)
        sys.exit(-1)

    # If there are spaces in the install path lots of commands need to be
    # patched so Python can be found on Linux/Mac.
    abs_bin = os.path.abspath(bin_dir)
    if not is_win and ' ' in abs_bin:
        import stat
        shebang = '#!"%%s"\\n' %% os.path.join(abs_bin, 'python')
        print '\\nFixing scripts for spaces in install path'
        for path in sorted(glob.glob(os.path.join(bin_dir, '*'))):
            with open(path, 'r') as script:
                lines = script.readlines()
            if lines[0] == shebang:
                mode = os.stat(path).st_mode
                os.rename(path, path+'.orig')
                lines[0] = '#!/usr/bin/env python\\n'
                with open(path, 'w') as script:
                    script.writelines(lines)
                os.chmod(path, mode)

    abshome = os.path.abspath(home_dir)

    if failures:
        failures.sort()
        print '\\n\\n***** The following packages failed to install: %%s.' %% failures
        print
        print 'This may be an intermittent network problem and simply retrying'
        print 'could result in a successfull installation.  Without all'
        print 'packages at least some tests will likely fail, and without core'
        print 'packages such as Traits OpenMDAO will not function at all.'
        print
        if not activated:
            print 'If you would like to try using this installation anyway,'
            print 'from %%s type:\\n' %% abshome
            if is_win:
                print r'Scripts\\activate'
            else:
                print '. bin/activate'
            print '\\nto activate your environment.'

    else:
        print '\\n\\nThe OpenMDAO virtual environment has been installed in\\n %%s' %% abshome
        if not activated:
            print '\\nFrom %%s, type:\\n' %% abshome
            if is_win:
                print r'Scripts\\activate'
            else:
                print '. bin/activate'
            print '\\nto activate your environment and start using OpenMDAO.'

    sys.exit(1 if failures else 0)
    """

    reqs = set()

    version = '?.?.?'
    excludes = set(['setuptools', 'distribute', 'SetupDocs']+openmdao_prereqs)
    dists = working_set.resolve([Requirement.parse(r[0])
                                   for r in openmdao_packages])

    distnames = set([d.project_name for d in dists])-excludes

    try:
        setupdoc_dist = working_set.resolve([Requirement.parse('setupdocs')])[0]
    except:
        setupdoc_dist = None

    for dist in dists:
        if dist.project_name not in distnames:
            continue
        if dist.project_name == 'openmdao.main':
            version = dist.version
        if options.dev:  # in a dev build, exclude openmdao stuff because we'll make them 'develop' eggs
            if not dist.project_name.startswith('openmdao.'):
                reqs.add('%s' % dist.as_requirement())
        else:
            reqs.add('%s' % dist.as_requirement())

    # adding setupdocs req is a workaround to prevent Traits from looking elsewhere for it
    if setupdoc_dist:
        _reqs = [str(setupdoc_dist.as_requirement())]
    else:
        _reqs = ['setupdocs>=1.0']
    reqs = sorted(_reqs + list(reqs))

    # pin setuptools to this version
    setuptools_version = "0.9.5"
    setuptools_url = "https://openmdao.org/dists/setuptools-%s-py%s.egg" % (setuptools_version, sys.version[:3])

    optdict = {
        'mkdir_pkg':         offline_[3],
        'extarg1':           offline_[0],
        'extarg2':           offline_[1],
        'dldir':             offline_[2],
        'cmds_str':          offline_[4],
        'openmdao_cmds_str': offline_[5],
        'reqs':              reqs,
        'version':           version,
        'url':               options.findlinks,
        'make_dev_eggs':     make_dev_eggs,
        'make_docs':         make_docs,
        'adjust_options':    _get_adjust_options(options, version, setuptools_url, setuptools_version),
        'openmdao_prereqs':  openmdao_prereqs,
    }

    dest = os.path.abspath(options.dest)
    if options.dev:
        scriptname = os.path.join(dest, f_prefix + 'go-openmdao-dev.py')
    else:
        scriptname = os.path.join(dest, f_prefix + 'go-openmdao-%s.py' % version)

    if os.path.isfile(scriptname):
        shutil.copyfile(scriptname, scriptname+'.old'
                        )
    with open(scriptname, 'wb') as f:
        # Pin the version of setuptools used.
        fixline = u"        egg_path = 'setuptools-*-py%s.egg' % sys.version[:3]"
        for line in virtualenv.create_bootstrap_script(script_str % optdict).split('\n'):
            if line == fixline:
                line = line.replace('*', setuptools_version)
            elif ", '-U'" in line:  # remove forcing it to look for latest setuptools version
                line = line.replace(", '-U'", "")
            f.write('%s\n' % line)
    os.chmod(scriptname, 0755)


if __name__ == '__main__':
    main()
