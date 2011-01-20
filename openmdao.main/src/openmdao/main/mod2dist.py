"""
A script to package into a distribution a single module containing 
OpenMDAO plugin classes. The script imports the module in order to 
determine its contents, so all dependencies must be on sys.path.
"""

import sys
import os
import os.path
import inspect
import StringIO
import pprint
import shutil
import tempfile
import logging
import copy
from subprocess import check_call, Popen, PIPE, STDOUT
from optparse import OptionParser

import pkg_resources
# easy install barfs without distutils import
import distutils.command.build
import setuptools.command.easy_install
from enthought.traits.api import TraitType

from openmdao.main.api import Component, Driver # , ResourceAllocator, CaseIterator

logging.basicConfig(level=logging.INFO)

class Mod2DistError(RuntimeError):
    def __init__(self, msg, parser=None):
        super(Mod2DistError, self).__init__(msg)
        self.parser = parser

def _find_dist(path, pkgname, version):
    env = pkg_resources.Environment(path)
    for dist in env[pkgname]:
        if dist.version == version:
            return dist.location        
    return None

def _run_command(cmd):
    """Run a command using Popen and return its output (stdout and stderr)
    and its return code as a tuple. If the command is a python file, prepend
    python to the command string. cmd should be a sequence of strings.
    """
    p = Popen(args=cmd, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=False)
    output = p.communicate()[0]
    return (output, p.returncode)


def mod2dist(argv=None, groups= { 'openmdao.component': Component,
                            'openmdao.driver': Driver,
                            #'openmdao.case_iterator': CaseIterator,
                            #'openmdao.resource_allocator': ResourceAllocator,
                            'openmdao.variable': TraitType
                            }):
    """Utility to simplify the packaging of a single module containing OpenMDAO
    plugin classes into a source distribution.
    
    Use mod2dist -h to see all of the options.
    
    The only required options are the desired version of the distribution and 
    the module to use to generate the distribution.  For example:

    mod2dist -v 1.0 simple_adder.py
    """
    parser = OptionParser()
    parser.usage = "mod2dist.py [options] <module_name>"
    parser.add_option("-v","--version", action="store", type="string", dest="version",
                      help="specify the version number (label) for the distribution")

    parser.add_option("-d","--dest", action="store", type="string", dest="dest",
                      help="specify the destination directory for the distribution")

    parser.add_option("--desc", action="store", type="string", dest="desc",
                      help="specify a description for the distribution")

    parser.add_option("-a","--author", action="store", type="string", dest="author",
                      help="specify the author of the distribution")
    
    parser.add_option("-e","--email", action="store", type="string", dest="author_email",
                      help="specify a contact email for the distribution")
    
    parser.add_option("-i","--install_dir", action="store", type="string", dest="install_dir",
                      help="install the distribution in the specified directory")
    
    parser.add_option("-l","--license", action="store", type="string", dest="license",
                      help="specify a license for all files in the distribution")
    
    parser.add_option("-u","--url", action="store", type="string", dest="url",
                      help="specify URL of the web page for the distribution")
    
    parser.add_option("--verbose", action="store_true", dest="verbose", default=False,
                      help="generate verbose output while building")
    
    parser.add_option("-k","--keep", action="store_true", dest="keep", default=False,
                      help="keep the package directory structure used to build the distribution")
    
    parser.add_option("-n","", action="store_true", dest="nodist", default=False,
                      help="don't actually build a distribution")
    
    if not argv:
        argv = sys.argv[1:]
    (options, args) = parser.parse_args(argv)

    if len(args) == 0:
        raise Mod2DistError('No module specified', parser)
    elif len(args) > 1:
        raise Mod2DistError('Only one module is allowed', parser)

    if not os.path.exists(args[0]):
        raise Mod2DistError("module %s does not exist" % args[0], parser)
        
    if not args[0].endswith('.py'):
        raise Mod2DistError("%s is not a python module" % args[0], parser)

    if not options.version:
        raise Mod2DistError("distribution version has not been specified", 
                           parser)
        
    modname = os.path.basename(os.path.splitext(args[0])[0])
    modpath = os.path.abspath(os.path.dirname(args[0]))
    
    old_sys_path = copy.copy(sys.path)
    old_sys_modules = sys.modules.copy()
    
    sys.path = [modpath]+sys.path
    
    if options.install_dir:
        if not os.path.isdir(options.install_dir):
            raise Mod2DistError("install directory %s does not exist\n" % options.install_dir)
        ename = _find_dist([options.install_dir], modname, options.version)
        # be a little extra paranoid about accidental overwriting of a
        # distribution without updating its version
        if ename:
            raise Mod2DistError("distrib %s already exists in directory %s" %\
                  (ename, os.path.abspath(options.install_dir)))
        if os.path.isabs(options.install_dir):
            idir_abs = options.install_dir
        else:
            idir_abs = os.path.join(os.getcwd(), options.install_dir)
    else:
        idir_abs = None

    destdir = options.dest or os.getcwd()
    destdir = os.path.abspath(destdir)
    if options.keep:
        pkgdir = destdir
    else:
        pkgdir = tempfile.mkdtemp()

    ename = _find_dist([destdir], modname, options.version)
    # be a little extra paranoid about accidental overwriting of a
    # distribution without updating its version
    if ename:
        raise Mod2DistError("distrib %s already exists in directory %s" %\
              (ename, os.path.abspath(destdir)))
        
    mod = __import__(modname)

    plugins = groups.copy()
    for name in plugins.keys():
        plugins[name] = []

    othermods = set()

    # find any classes in the module so we can see if they're 
    # OpenMDAO plugin types
    for name, val in inspect.getmembers(mod):
        valmod = inspect.getmodule(val)
        if inspect.isclass(val):
            if valmod is mod:
                for gname, klass in groups.items():
                    if issubclass(val, klass):
                        plugins[gname].append(name)
        if valmod is not None and valmod is not mod and valmod.__name__ is not None:
            othermods.add(valmod)

    # for each module imported by our module, find the distrib that contains it 
    depends = set()
    for omod in othermods:
        found = False
        for dist in pkg_resources.working_set:
            if omod.__file__.startswith(dist.location):
                found = True
                break
        if found:
            break
            
    # now put sys.path and sys.modules back to the way they were
    sys.path = old_sys_path
    sys.modules = old_sys_modules
                    
    orig_dir = os.getcwd()
    
    # create package dir
    os.makedirs(os.path.join(pkgdir, modname, modname))
    os.chdir(os.path.join(pkgdir, modname))

    # create the entry point dict
    entrypts = {}
    classnames = set()
    try:
        for group,pulist in plugins.items():
            if len(pulist) > 0:
                if group not in entrypts: 
                    entrypts[group] = []
                for pu in pulist:
                    entrypts[group].append('%s = %s:%s' % (pu,modname,pu))
                    classnames.add(pu)
 
        setup_template = '''
from setuptools import setup

setup(
    name='%(name)s',
    version='%(version)s',
    description=%(desc)s,
    author=%(author)s,
    author_email=%(author_email)s,
    license=%(license)s,
    url=%(url)s,
    packages=['%(name)s'],
    zip_safe=%(zipped)s,
    install_requires=%(depends)s,
    entry_points=%(entrypts)s
)   
   '''
        f = open('setup.py', 'w')
        f.write(setup_template % { 'name': modname, 
                                   'version': options.version,
                                   'desc': options.desc,
                                   'author': options.author,
                                   'author_email': options.author_email,
                                   'license': options.license,
                                   'url': options.url,
                                   'entrypts': entrypts,
                                   'zipped': False,
                                   'depends': list(depends) })
        f.close()
        
        shutil.copy(os.path.join(orig_dir,args[0]), 
                    os.path.join(pkgdir, modname, modname, os.path.basename(args[0])))
        f = open(os.path.join(pkgdir, modname, modname, '__init__.py'), 'w')
        f.write('from %s import %s' % (modname, ','.join(classnames)))
        f.close()
        
        # build the distrib
        if not options.nodist:
            cmdargs = [sys.executable, 
                                    'setup.py', 'sdist', '-d', destdir]
            out, ret = _run_command(cmd=cmdargs)
            if ret:
                logging.error('non-zero return code (%s) from command: %s' % 
                              (ret,' '.join(cmdargs)))
                print out
            elif options.verbose:
                print out
            if sys.platform == 'win32':
                distname = '%s-%s.zip' % (modname, options.version)
            else:
                distname = '%s-%s.tar.gz' % (modname,options.version)
            logging.info('created distribution %s in %s' % (distname, destdir))
            if idir_abs:
                # find the distribution we just built
                if distname not in os.listdir(destdir):
                    raise RuntimeError("ERROR: cannot locate distribution file")
            
                os.chdir(idir_abs)
                if options.verbose:
                    optstr = '-mN'
                else:
                    optstr = '-mNq'
                setuptools.command.easy_install.main(
                    argv=['-d','.',optstr,'%s' % os.path.join(destdir, distname)])
            
                logging.info('installed %s in %s' % (distname, idir_abs)) 
            try:
                shutil.rmtree(os.path.join(pkgdir, modname, 'build'))
            except OSError:
                pass
            shutil.rmtree(os.path.join(pkgdir, modname, modname+'.egg-info'))
    finally:
        os.chdir(orig_dir)
        if not options.keep:
            shutil.rmtree(pkgdir)
        
    return 0
   
if __name__ == "__main__": # pragma no cover
    try:
        sys.exit(mod2dist(sys.argv[1:]))
    except Mod2DistError, err:
        sys.stderr.write(str(err)+'\n')
        if err.parser:
            err.parser.print_help()
        sys.exit(-1)
   
