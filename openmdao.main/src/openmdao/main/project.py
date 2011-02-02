"""
Routines for handling 'projects' in python.
"""

import os
import sys

def model_to_class(model, classname, stream):
    """Takes a model and creates a new class inherited from the model's
    class that is initialized with the current model's non-default
    configuration.  The class definition is written to the given stream.
    Note that this does not save the exact state of the model, but only
    key inputs and attributes recommended by the model and its children.
    """
    cfg = model.get_configinfo()
    cfg.save_as_class(stream, classname)

def model_to_package(model, classname, version, destdir='.'):
    startdir = os.getcwd()
    try:
        os.chdir(destdir)
        pkgname = classname.lower()
        os.makedirs(os.path.join(pkgname, pkgname))
        fname = os.path.join(pkgname, pkgname, "%s.py" % pkgname)
        with open(fname, 'w') as f:
            model_to_class(model, classname, f)
        make_pkg('.', classname, version=version)
    finally:
        os.chdir(startdir)

def create_setup_py_file(stream, header='', footer='', **kwargs):
    """Writes the contents of a setup.py file to the given stream.
    kwargs contains options to the setup command. Entries of
    'name' (the distribution name) and 'version' are required. Others
    (desc,author,author_email,license,url,depends,entrypts) are optional.
    """
    
    setuptemplate = '''
from setuptools import setup

setup(
    name='%(name)s',
    version='%(version)s',
    description='%(desc)s',
    author='%(author)s',
    author_email='%(author_email)s',
    license='%(license)s',
    url='%(url)s',
    packages=['%(name)s'],
    zip_safe=%(zipped)s,
    install_requires=%(depends)s,
    entry_points=%(entrypts)s
)

   '''
    
    setupdict = { 'name': kwargs['name'],
                  'version': kwargs['version'],
                  'desc': kwargs.get('desc', ''),
                  'author': kwargs.get('author', ''),
                  'author_email': kwargs.get('author_email', ''),
                  'license': kwargs.get('license', ''),
                  'url': kwargs.get('url', ''),
                  'entrypts': kwargs.get('entrypts', {}),
                  'zipped': False,
                  'depends': kwargs.get('depends',[]),
                  }
    
    stream.write("%s\n" % header)
    stream.write(setuptemplate % setupdict)
    stream.write("\n%s\n" % footer)
    
def new_project(destdir, name, version='0.0.1', 
                base='openmdao.main.assembly.Assembly', **kwargs):
    """Creates a directory structure, default setup.py file, and __init__.py
    file in the specified destination directory for a project (python package)
    with the given name. Optional keyword arguments to the setup call can be
    supplied in kwargs.
    """
    if not os.path.exists(destdir):
        raise IOError("destination directory '%s' doesn't exist" % destdir)
    pkgname = name.lower()
    os.makedirs(os.path.join(destdir, pkgname, pkgname))
    header = """
    # GENERATED FILE - DO NOT EDIT!
    """
    kwargs['name'] = pkgname
    kwargs['version'] = version
    with open(os.path.join(destdir, pkgname, 'setup.py'), 'w') as f:
        create_setup_py_file(f, header, **kwargs)
    with open(os.path.join(destdir, pkgname, pkgname, '%s.py' % pkgname), 'w') as f:
        module, cname = base.rsplit('.', 1)
        f.write("""
from %s import %s

class %s(%s):
    pass
    
        """ % (module, cname, name, cname))
    with open(os.path.join(destdir, pkgname, pkgname, '__init__.py'), 'w') as f:
        f.write("""
from %s import %s
        """ % (pkgname, name))

def update_package(pkgpath, **kwargs):
    """Updates the setup.py file in the package with the given location.
    Options to the setup call inside of setup.py can be passed in kwargs.
    """
    destdir, pkgname = os.path.split(pkgpath)
    
    # find plugins and create entry points
    sta = PythonSourceTreeAnalyser(startdir=pkgpath, excludes=['setup.py'])
    drivers = sta.find_inheritors('openmdao.main.driver.Driver')
    comps = sta.find_inheritors('openmdao.main.component.Component')
    comps = list(set(comps)-set(drivers))
    drv_entrypts = []
    comp_entrypts = []
    for drv in drivers:
        parts = drv.split('.')
        drv_entrypts.append("%s=%s:%s" % (drv,'.'.join(parts[:-1]),parts[-1]))
    for comp in comps:
        parts = comp.split('.')
        comp_entrypts.append("%s=%s:%s" % (comp,'.'.join(parts[:-1]),parts[-1]))
    if drv_entrypts:
        setupdict['entrypts']['openmdao.drivers'] = drv_entrypts
    if comp_entrypts:
        setupdict['entrypts']['openmdao.components'] = comp_entrypts
        
    print 'done'
