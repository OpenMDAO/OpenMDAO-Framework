import sys
import os
import tempfile
import shutil
import subprocess
from optparse import OptionParser

from openmdao.util.fileutil import build_directory

_setup_py_template = '''

from setuptools import setup

setup(
   name = '%(name)s',
   version = '%(version)s',
   packages = ['%(name)s'],
   package_dir = {'': 'src'},
   zip_safe = False,
   install_requires = %(requires)s,
   dependency_links = %(deplinks)s,
)

'''


def mkpseudo(argv=None):
    """A command line script (mkpseudo) points to this.  It generates a
    source distribution package that's empty aside from
    having a number of dependencies on other packages.
    
    usage: make_pseudopkg -n <pkg_name> -v <version> [-d <dest_dir>] [-l <links_url>] req1 ... req_n
    
    Required dependencies are specified using the same notation used by
    setuptools/easy_install/distribute/pip
    
    NOTE: If your required dependencies use the '<' or '>' characters you must put the
    entire requirement in quotes to avoid misinterpretation by the shell.

    """
    
    if argv is None:
        argv = sys.argv[1:]
    
    parser = OptionParser()
    parser.usage = "mkpseudo [options] [requirements]"
    parser.add_option("-n", "--name", action="store", type="string", dest='name',
                      help="name of the package (required)")
    parser.add_option("-v", "--version", action="store", type="string", dest='version',
                      help="version id of the package (required)")
    parser.add_option("-d", "--dest", action="store", type="string", dest='dest', default='.',
                      help="directory where distribution will be created (optional)")
    parser.add_option("-l", "--link", action="append", type="string", dest='deplinks', default=[],
                      help="URLs to search for dependencies (optional)")
    
    (options, args) = parser.parse_args(argv)

    name = options.name
    version = options.version
    
    if not name or not version:
        parser.print_help()
        sys.exit(-1)
    
    for i,url in enumerate(options.deplinks):
        if not url.startswith('http:') and not url.startswith('https:'):
            options.deplinks[i] = "http://%s" % url

    options.dest = os.path.abspath(os.path.expandvars(os.path.expanduser(options.dest)))

    if len(args) == 0:
        print "No dependencies have been specified, so no pseudo package will be created"
        sys.exit(-1)

    setup_options = {
        'requires': args,
        'name': name,
        'version': version,
        'deplinks': options.deplinks,
        }
    
    startdir = os.getcwd()
    tdir = tempfile.mkdtemp()
    
    try:
        os.chdir(tdir)
        
        dirstruct = {
            'setup.py': _setup_py_template % setup_options,
            'src': {
                name: {
                    '__init__.py': '',
                    },
                },
        }

        build_directory(dirstruct)
        
        dest = os.path.expanduser(os.path.expandvars(options.dest))
        tarname = os.path.join(dest, "%s-%s.tar.gz" % (name,version))
        zipname = os.path.join(dest, "%s-%s.zip" % (name,version))
        for fname in [tarname, zipname]:
            if os.path.exists(fname):
                print "%s already exists" % fname
                sys.exit(-1)
        
        cmd = [sys.executable, 'setup.py', 'sdist', '-d', options.dest]
        subprocess.check_call(cmd)
        
    finally:
        os.chdir(startdir)
        shutil.rmtree(tdir)


if __name__ == '__main__':
    mkpseudo(sys.argv[1:])
    
