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
from subprocess import check_call
from optparse import OptionParser

import pkg_resources
import zc.buildout
from enthought.traits.api import TraitType

from openmdao.main.api import Component, Driver # , ResourceAllocator, CaseIterator

def _find_egg(path, pkgname, version):
    env = pkg_resources.Environment(path)
    for dist in env[pkgname]:
        if dist.version == version:
            return dist.egg_name()+'.egg'
    return None

def mod2egg(argv):
    parser = OptionParser()
    parser.usage = "mod2egg.py [options] <module_name>"
    parser.add_option("-v","--version", action="store", type="string", dest="version",
                      help="specify the version number (label) for the egg")

    parser.add_option("-d","--dest", action="store", type="string", dest="dest",
                      help="specify the destination directory for the egg")

    parser.add_option("","--desc", action="store", type="string", dest="desc",
                      help="specify a description for the egg")

    parser.add_option("-a","--author", action="store", type="string", dest="author",
                      help="author of the module")
    
    parser.add_option("-i","--install_dir", action="store", type="string", dest="install_dir",
                      help="install the egg in the specified directory")
    
    parser.add_option("-z","--zipped_egg", action="store_true", dest="zipped", default=False,
                      help="zip safe")
    
    (options, args) = parser.parse_args(argv)

    if len(args) == 0:
        print 'ERROR: No module specified\n'
        parser.print_help()
        sys.exit(-1)
    elif len(args) > 1:
        print 'ERROR: Only one module is allowed\n'
        parser.print_help()
        sys.exit(-1)

    if not os.path.exists(args[0]):
        print "ERROR: module %s does not exist\n" % args[0]
        sys.exit(-1)
        
    if not args[0].endswith('.py'):
        print "ERROR: %s is not a python module\n" % args[0]
        sys.exit(-1)

    if not options.version:
        print "ERROR: Version of distribution has not been specified\n"
        parser.print_help()
        sys.exit(-1)
        
    modname = os.path.basename(os.path.splitext(args[0])[0])
    
    if options.install_dir:
        if not os.path.isdir(options.install_dir):
            print "ERROR: install directory %s does not exist\n" % options.install_dir
            sys.exit(-1)
        ename = _find_egg([options.install_dir], modname, options.version)
        # be a little extra paranoid about accidental overwriting of an
        # egg without updating its version
        if ename:
            print "ERROR: egg %s already exists in directory %s" %\
                  (ename, os.path.abspath(options.install_dir))
            sys.exit(-1)
        if os.path.isabs(options.install_dir):
            idir_abs = options.install_dir
        else:
            idir_abs = os.path.join(os.getcwd(), options.install_dir)
    else:
        idir_abs = None

    destdir = options.dest or os.getcwd()
    pkgdir = tempfile.mkdtemp()

    ename = _find_egg([destdir], modname, options.version)
    # be a little extra paranoid about accidental overwriting of an
    # egg without updating its version
    if ename:
        print "ERROR: egg %s already exists in directory %s" %\
              (ename, os.path.abspath(destdir))
        sys.exit(-1)
        
    sys.path[0:0] = '.'
    mod = __import__(modname)

    groups = { 'openmdao.component': Component,
                'openmdao.driver': Driver,
                #'openmdao.case_iterator': CaseIterator,
                #'openmdao.resource_allocator': ResourceAllocator,
                'openmdao.trait': TraitType
                }

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
                    
    orig_dir = os.getcwd()
    
    # create package dir
    os.makedirs(os.path.join(pkgdir, modname))
    os.chdir(os.path.join(pkgdir))

    # create the entry point string
    epstr = StringIO.StringIO()
    epstr.write('\n')
    try:
        for group,pulist in plugins.items():
            if len(pulist) > 0:
                epstr.write('    [%s]\n' % group)
                for pu in pulist:
                    epstr.write('    %s = %s:%s\n' % (pu,modname,pu))
        entrypts = epstr.getvalue()
        epstr.close()
 
        setup_template = '''
from setuptools import setup

setup(
    name='%(name)s',
    version='%(version)s',
    description=%(desc)s,
    author=%(author)s,
    packages=['%(name)s'],
    zip_safe=%(zipped)s,
    install_requires=%(depends)s,
    entry_points="""%(entrypts)s"""
)   
   '''
        f = open('setup.py', 'w')
        f.write(setup_template % { 'name': modname, 
                                   'version': options.version,
                                   'desc': options.desc,
                                   'author': options.author,
                                   'entrypts': entrypts,
                                   'zipped': options.zipped,
                                   'depends': list(depends) })
        f.close()
        sys.stdout.write(setup_template % { 'name': modname, 
                                   'version': options.version,
                                   'desc': options.desc,
                                   'author': options.author,
                                   'entrypts': entrypts,
                                   'zipped': options.zipped,
                                   'depends': list(depends) })

        # copy the given module into the package __init__.py file
        # to avoid an extra name in the path when using the egg
        shutil.copy(os.path.join(orig_dir,args[0]), 
                    os.path.join(pkgdir, modname, '__init__.py'))
        
        # build the egg (and if a zipped egg, put in install_dir)
        if idir_abs and options.zipped:
            check_call([sys.executable, 
                        'setup.py', 'bdist_egg', '-d', idir_abs])
        else:
            check_call([sys.executable, 
                        'setup.py', 'bdist_egg', '-d', destdir])
        
        if idir_abs and not options.zipped:
            # find the egg we just built
            eggname = _find_egg([destdir], modname, options.version)
            if not eggname:
                raise RuntimeError("ERROR: cannot locate egg file")
            
            os.chdir(idir_abs)
            check_call(['easy_install', '-d', '.', '-mNq', 
                        '%s' % os.path.join(destdir,eggname)])
            
            print 'installed %s in %s' % (eggname, destdir)
            
    finally:
        os.chdir(orig_dir)
        shutil.rmtree(pkgdir)
   
   
if __name__ == "__main__":
    mod2egg(sys.argv[1:])
   
