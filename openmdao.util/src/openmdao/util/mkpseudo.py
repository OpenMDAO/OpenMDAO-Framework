import sys
import os
import tempfile
import shutil
import subprocess
from optparse import OptionParser

from pkg_resources import working_set, Requirement

from openmdao.util.fileutil import build_directory, onerror

_setup_py_template = '''

from setuptools import setup, find_packages

setup(
   name = '%(name)s',
   version = '%(version)s',
   packages = find_packages('src'),
   package_dir = {'': 'src'},
   namespace_packages=%(nspkgs)s,
   zip_safe = False,
   install_requires = %(requires)s,
   dependency_links = %(deplinks)s,
   #long_description="",
   #classifiers=[
   #     'Intended Audience :: Science/Research',
   #     'Topic :: Scientific/Engineering',
   #],
   #keywords='optimization multidisciplinary multi-disciplinary analysis',
   #author='',
   #author_email='',
   #url='http://???',
   #license='Apache License, Version 2.0',
   #test_suite='nose.collector',
)

'''

_ns_template = '''
# this is a namespace package
try:
    import pkg_resources
    pkg_resources.declare_namespace(__name__)
except ImportError:
    import pkgutil
    __path__ = pkgutil.extend_path(__path__, __name__)
'''


def mkpseudo(argv=None):
    """A command line script (mkpseudo) points to this.  It generates a
    source distribution package that's empty aside from
    having a number of dependencies on other packages.

    usage: ``make_pseudopkg <pkg_name> <version> [-d <dest_dir>] [-l <links_url>] [-r req1] ... [-r req_n]``

    If ``pkg_name`` contains dots, a namespace package will be built.

    Required dependencies are specified using the same notation used by
    ``setuptools/easy_install/distribute/pip``.

    .. note:: If your required dependencies use the "<" or ">" characters, you must put the
              entire requirement in quotes to avoid misinterpretation by the shell.

    """

    if argv is None:
        argv = sys.argv[1:]

    parser = OptionParser()
    parser.usage = "mkpseudo <name> <version> [options]"
    parser.add_option("-d", "--dest", action="store", type="string", dest='dest', default='.',
                      help="directory where distribution will be created (optional)")
    parser.add_option("-l", "--link", action="append", type="string", dest='deplinks', default=[],
                      help="URLs to search for dependencies (optional)")
    parser.add_option("-r", "--req", action="append", type="string", dest='reqs', default=[],
                      help="requirement strings for dependent distributions (one or more)")
    parser.add_option("", "--dist", action="store_true", dest="dist",
                      help="make a source distribution after creating the directory structure")

    (options, args) = parser.parse_args(argv)

    if len(args) != 2:
        parser.print_help()
        sys.exit(-1)

    name = args[0]
    names = name.split('.')
    version = args[1]

    for i, url in enumerate(options.deplinks):
        if not url.startswith('http:') and not url.startswith('https:'):
            options.deplinks[i] = "http://%s" % url

    dest = os.path.abspath(os.path.expandvars(os.path.expanduser(options.dest)))

    if len(options.reqs) == 0 and options.dist:
        print "No dependencies have been specified, so the distribution will not be built"
        options.dist = False

    nspkgs = []
    for i, nm in enumerate(names[:-1]):
        nspkgs.append('.'.join(names[:i+1]))

    dists = working_set.resolve([Requirement.parse(r) for r in options.reqs])
    dset = set([("%s" % d).replace(' ','==') for d in dists])

    setup_options = {
        'requires': list(dset),
        'name': name,
        'version': version,
        'deplinks': options.deplinks,
        'nspkgs': nspkgs,
    }

    startdir = os.getcwd()
    if options.dist:
        tdir = tempfile.mkdtemp()
    else:
        tdir = dest

    try:
        os.chdir(tdir)

        rnames = names[::-1]
        for i, ns in enumerate(rnames):
            if i == 0:
                dct = {'__init__.py': ''}
            else:
                dct = {
                    '__init__.py': _ns_template,
                    rnames[i-1]: dct,
                }

        dct = {names[0]: dct}

        dirstruct = {
            name: {
                'setup.py': _setup_py_template % setup_options,
                'src': dct,
            },
        }

        if not options.dist:
            if os.path.exists(name):
                print "'%s' already exists.  aborting..." % name
                sys.exit(-1)

        build_directory(dirstruct)

        os.chdir(name)

        if options.dist:
            tarname = os.path.join(dest, "%s-%s.tar.gz" % (name, version))
            zipname = os.path.join(dest, "%s-%s.zip" % (name, version))
            for fname in [tarname, zipname]:
                if os.path.exists(fname):
                    print "%s already exists" % fname
                    sys.exit(-1)

            cmd = [sys.executable, 'setup.py', 'sdist', '-d', dest]
            subprocess.check_call(cmd)

    finally:
        os.chdir(startdir)
        if options.dist:
            shutil.rmtree(tdir, onerror=onerror)


if __name__ == '__main__':
    mkpseudo(sys.argv[1:])
