"""
Routines for handling 'projects' in python.
"""

import os
import sys


def model_to_package(model, classname, version, destdir='.'):
    startdir = os.getcwd()
    try:
        os.chdir(destdir)
        pkgname = classname.lower()
        os.makedirs(os.path.join(pkgname, pkgname))
        fname = os.path.join(pkgname, pkgname, "%s.py" % pkgname)
        with open(fname, 'w') as f:
            cfg = model.get_configinfo()
            cfg.save_as_class(f, classname)
        make_pkg('.', classname, version=version)
    finally:
        os.chdir(startdir)

def create_setup_py_file(destdir, pkgname, **kwargs):
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
    
    setupdict = { 'name': pkgname,
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
    
    startdir = os.getcwd()
    os.chdir(destdir)
    try:
        with open(os.path.join(pkgname, 'setup.py'), 'w') as f:
            f.write("""
    
    # GENERATED FILE - DO NOT EDIT!
    
                    """)
            f.write(setuptemplate % setupdict)
    finally:
        os.chdir(startdir)
    
def new_package(destdir, pkgname, **kwargs):
    """Creates a directory structure, default setup.py file, and __init__.py
    file in the specified destination directory for a package with the given name.
    """
    if not os.path.exists(destdir):
        raise IOError("destination directory '%s' doesn't exist" % destdir)
    os.makedirs(os.path.join(destdir, pkgname, pkgname))
    create_setup_py_file(destdir, pkgname, **kwargs)
    f = open(os.path.join(destdir, pkgname, pkgname, '__init__.py'), 'w')
    f.close()

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
