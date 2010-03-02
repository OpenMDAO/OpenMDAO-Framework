"""
A script to package into an egg a single module containing OpenMDAO plugin
classes. The script imports the module in order to determine its contents,
so all dependencies must be on sys.path.
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
from subprocess import check_call
from optparse import OptionParser

import pkg_resources
# easy install barfs without distutils import
import distutils.command.build
import setuptools.command.easy_install
import zc.buildout
from enthought.traits.api import TraitType

from openmdao.main.api import Component, Driver # , ResourceAllocator, CaseIterator

logging.basicConfig(level=logging.INFO)

class Mod2EggError(RuntimeError):
    def __init__(self, msg, parser=None):
        super(Mod2EggError, self).__init__(msg)
        self.parser = parser

def _find_egg(path, pkgname, version):
    env = pkg_resources.Environment(path)
    for dist in env[pkgname]:
        if dist.version == version:
            return dist.egg_name()+'.egg'
    return None

def mod2egg(argv, groups= { 'openmdao.component': Component,
                            'openmdao.driver': Driver,
                            #'openmdao.case_iterator': CaseIterator,
                            #'openmdao.resource_allocator': ResourceAllocator,
                            'openmdao.trait': TraitType
                            }):
    parser = OptionParser()
    parser.usage = "mod2egg.py [options] <module_name>"
    parser.add_option("-v","--version", action="store", type="string", dest="version",
                      help="specify the version number (label) for the egg")

    parser.add_option("-d","--dest", action="store", type="string", dest="dest",
                      help="specify the destination directory for the egg")

    parser.add_option("","--desc", action="store", type="string", dest="desc",
                      help="specify a description for the egg")

    parser.add_option("-a","--author", action="store", type="string", dest="author",
                      help="specify the author of the egg")
    
    parser.add_option("-e","--email", action="store", type="string", dest="author_email",
                      help="specify a contact email for the egg")
    
    parser.add_option("-i","--install_dir", action="store", type="string", dest="install_dir",
                      help="install the egg in the specified directory")
    
    parser.add_option("-l","--license", action="store", type="string", dest="license",
                      help="specify a license for all files in the egg")
    
    parser.add_option("-u","--url", action="store", type="string", dest="url",
                      help="specify URL of the web page for the egg")
    
    parser.add_option("-z","--zipped_egg", action="store_true", dest="zipped", default=False,
                      help="egg is zip safe")
    
    parser.add_option("","--verbose", action="store_true", dest="verbose", default=False,
                      help="generate verbose output while building")
    
    parser.add_option("-k","--keep", action="store_true", dest="keep", default=False,
                      help="keep the package directory structure used to build the egg")
    
    parser.add_option("-n","--noegg", action="store_true", dest="noegg", default=False,
                      help="don't actually build an egg")
    
    (options, args) = parser.parse_args(argv)

    if len(args) == 0:
        raise Mod2EggError('No module specified', parser)
    elif len(args) > 1:
        raise Mod2EggError('Only one module is allowed', parser)

    if not os.path.exists(args[0]):
        raise Mod2EggError("module %s does not exist" % args[0], parser)
        
    if not args[0].endswith('.py'):
        raise Mod2EggError("%s is not a python module" % args[0], parser)

    if not options.version:
        raise Mod2EggError("distribution version has not been specified", 
                           parser)
        
    modname = os.path.basename(os.path.splitext(args[0])[0])
    modpath = os.path.abspath(os.path.dirname(args[0]))
    
    old_sys_path = copy.copy(sys.path)
    old_sys_modules = sys.modules.copy()
    
    sys.path = [modpath]+sys.path
    
    if options.install_dir:
        if not os.path.isdir(options.install_dir):
            raise Mod2EggError("install directory %s does not exist\n" % options.install_dir)
        ename = _find_egg([options.install_dir], modname, options.version)
        # be a little extra paranoid about accidental overwriting of an
        # egg without updating its version
        if ename:
            raise Mod2EggError("egg %s already exists in directory %s" %\
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

    ename = _find_egg([destdir], modname, options.version)
    # be a little extra paranoid about accidental overwriting of an
    # egg without updating its version
    if ename:
        raise Mod2EggError("egg %s already exists in directory %s" %\
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
            othermods.add(valmod.__name__)

    # for each module imported by our module, find the distrib that contains it 
    depends = set()
    for omod in othermods:
        found = False
        for dist in pkg_resources.working_set:
            path = omod.replace('.','/')
            for ext in ['.py','.pyc','.pyd']:
                fname = path+ext
                try:
                    if dist.has_resource(fname):
                        depends.add(str(dist.as_requirement()))
                        found = True
                        break
                except AttributeError:
                    pass  # some stdlib dists barf when has_resource is called
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
                                   'zipped': options.zipped,
                                   'depends': list(depends) })
        f.close()
        shutil.copy(os.path.join(orig_dir,args[0]), 
                    os.path.join(pkgdir, modname, modname, os.path.basename(args[0])))
        f = open(os.path.join(pkgdir, modname, modname, '__init__.py'), 'w')
        f.write('from %s import %s' % (modname, ','.join(classnames)))
        f.close()
        
        # build the egg (and if a zipped egg, put in install_dir)
        if options.verbose:
            qstr = '--verbose'
        else:
            qstr = '--quiet'
            
        if not options.noegg:
            if idir_abs and options.zipped:
                check_call([sys.executable, 
                            'setup.py', qstr, 'bdist_egg', '-d', idir_abs])
                eggname = _find_egg([idir_abs], modname, options.version)
                logging.info('installed %s (zipped) in %s' % (eggname, idir_abs))
            else:
                check_call([sys.executable, 
                            'setup.py', qstr, 'bdist_egg', '-d', destdir])
                eggname = _find_egg([destdir], modname, options.version)
                logging.info('created egg %s in %s' % (eggname, destdir))
                if idir_abs:
                    # find the egg we just built
                    if not eggname:
                        raise RuntimeError("ERROR: cannot locate egg file")
                
                    os.chdir(idir_abs)
                    if options.verbose:
                        optstr = '-mN'
                    else:
                        optstr = '-mNq'
                    setuptools.command.easy_install.main(
                        argv=['-d','.',optstr,'%s' % os.path.join(destdir, eggname)])
                
                    logging.info('installed %s in %s' % (eggname, idir_abs)) 
            shutil.rmtree(os.path.join(pkgdir, modname, 'build'))
            shutil.rmtree(os.path.join(pkgdir, modname, modname+'.egg-info'))
    finally:
        os.chdir(orig_dir)
        if not options.keep:
            shutil.rmtree(pkgdir)
        
    return 0
   
if __name__ == "__main__":
    try:
        sys.exit(mod2egg(sys.argv[1:]))
    except Mod2EggError, err:
        sys.stderr.write(str(err)+'\n')
        if err.parser:
            err.parser.print_help()
        sys.exit(-1)
   
