import sys
import os
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


def main(argv=None):
    """A command line script (make_pseudopkg) points to this.  It generates a
    directory structure for an openmdao plugin package that's empty aside from
    having a number of dependencies on other packages.
    
    usage: make_pseudopkg <pseudo_pkg_name> [-v <version>] [-d <dest_dir>]
    
    """
    
    if argv is None:
        argv = sys.argv[1:]
    
    parser = OptionParser()
    parser.usage = "make_pseudopkg <pseudo_pkg_name> [options] [requirements]"
    parser.add_option("-n", "--name", action="store", type="string", dest='name',
                      help="name of the package (required)")
    parser.add_option("-v", "--version", action="store", type="string", dest='version',
                      help="version id of the package (required)")
    parser.add_option("-d", "--dest", action="store", type="string", dest='dest', default='.',
                      help="directory where package will be created (optional)")
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

    setup_options = {
        'requires': args,
        'name': name,
        'version': version,
        'deplinks': options.deplinks,
        }
    
    startdir = os.getcwd()
    try:
        os.chdir(options.dest)
        
        if os.path.exists(name):
            raise OSError("Can't create directory '%s' because it already exists." %
                          os.path.join(options.dest, name))

        dirstruct = {
            name: {
                'setup.py': _setup_py_template % setup_options,
                'src': {
                    name: {
                        '__init__.py': '', #'from %s import %s\n' % (name,classname),
                        },
                    },
            },
        }

        build_directory(dirstruct)
    
    finally:
        os.chdir(startdir)


if __name__ == '__main__':
    main(sys.argv[1:])