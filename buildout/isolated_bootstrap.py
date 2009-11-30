"""Bootstrap a buildout-based project

This is a modified version of the bootstrap.py file (Copyright (c) 2006 
Zope Corporation and Contributors) that is
part of zc.buildout.  This version creates a bin/buildout script that is 
isolated from the system level installed packages.

Simply run this script in a directory containing a buildout.cfg.
The script accepts buildout command-line options, so you can
use the -c option to specify an alternate configuration file.

$Id$
"""

import os, shutil, sys, tempfile #, urllib2
import fnmatch

bodir = os.getcwd()
setupdir = os.path.join(bodir,'setup')

stoolspat = "setuptools-*-py%s.egg" % sys.version[:3]
buildoutpat = "zc.buildout-*.tar.gz"
boeggpat = "zc.buildout-*-py%s.egg" % sys.version[:3]
receggpat = "zc.recipe.egg-*.tar.gz"
receggeggpat = "zc.recipe.egg-*-py%s.egg" % sys.version[:3]

sorted_dir = sorted(os.listdir(setupdir))
stools = fnmatch.filter(sorted_dir, stoolspat)
bouts = fnmatch.filter(sorted_dir, buildoutpat)
receggs = fnmatch.filter(sorted_dir, receggpat)
                
if len(stools)==0:
    sys.stderr.write('Missing setuptools distrib needed for bootstrapping')
    sys.exit(-1)
if len(bouts)==0:
    sys.stderr.write('Missing zc.buildout distrib needed for bootstrapping')
    sys.exit(-1)
if len(receggs)==0:
    sys.stderr.write('Missing zc.recipe.egg distrib needed for bootstrapping')
    sys.exit(-1)
 
# take the last setuptools in the sorted list, assuming it's
# the most recent version
stoolsname = stools.pop()

if os.path.basename(bodir) != 'buildout':
    sys.stderr.write('You must run this script from the buildout directory\n')
    sys.exit(-1)

# add paths for builtin python stuff (but no site-packages)                   
sys.path = [x for x in sys.path if 'site-packages' not in x]

# put setuptools on sys.path so we can import pkg_resources
sys.path.insert(0, os.path.join(bodir, 'setup', stoolsname))

import pkg_resources

if sys.platform == 'win32':
    def quote(c):
        if ' ' in c:
            return '"%s"' % c # work around spawn lamosity on windows
        else:
            return c
else:
    def quote (c):
        return c

cmd = "import sys; sys.path.insert(0,r'%s'); from setuptools.command.easy_install import main; main()" % os.path.join(bodir, 'setup', stoolsname)

assert os.spawnle(
    os.P_WAIT, sys.executable, quote (sys.executable),
    '-c', quote (cmd), '-H', 'None', '-f', setupdir, '-maqNxd', 
    quote (setupdir), 'zc.buildout',
    dict(os.environ,
         PYTHONPATH=setupdir
         ),
    ) == 0

ws  = pkg_resources.working_set
dist = pkg_resources.Environment([setupdir]).best_match(
                      pkg_resources.Requirement.parse('zc.buildout'),
                      ws)
ws.add_entry(dist.location)
ws.require('zc.buildout')
import zc.buildout.buildout

# instead of calling zc.buildout.buildout.main, we create a Buildout
# object ourselves so we can figure out where the eggs-directory is                                
try:
    command = 'bootstrap'
    buildout = zc.buildout.buildout.Buildout('buildout.cfg', cloptions=[],
                        user_defaults=True, windows_restart=False, command=command)
    getattr(buildout, command)(sys.argv[1:])
except SystemExit:
    pass

eggdir = buildout['buildout']['eggs-directory']

#zc.buildout.buildout.main(sys.argv[1:] + ['bootstrap'])

# make sure we have zc.recipe.egg
assert os.spawnle(
    os.P_WAIT, sys.executable, quote (sys.executable),
    '-c', quote (cmd), '-H', 'None', '-f', setupdir, '-maqNxd', 
    quote (eggdir), 'zc.recipe.egg',
    dict(os.environ,
         PYTHONPATH=setupdir
         ),
    ) == 0

# now modify the bin/buildout script to isolate it

old_sp = 'import zc.buildout.buildout'
new_sp = """
import os
prefx = os.path.join(sys.prefix,'lib','python'+sys.version[0:3])
sys.path[2:] = [  prefx+'.zip',
                 prefx,
                 os.path.join(prefx,'lib-dynload'),
                 os.path.join(prefx,'plat-'+sys.platform),
               ]
              
import zc.buildout.buildout
import zc.buildout.easy_install

# monkey patch zc.buildout.easy_install._script and _pyscript to change 
# the chmod from 0755 to 0775
_zc_easy_install_script = zc.buildout.easy_install._script
_zc_easy_install_pyscript = zc.buildout.easy_install._pyscript

def _script(module_name, attrs, path, dest, executable, arguments,
            initialization, rsetup):
    gen = _zc_easy_install_script(module_name, attrs, path, dest, 
                                  executable, arguments,
                                  initialization, rsetup)
    try:
        os.chmod(dest, 0775)
    except (AttributeError, os.error):
        pass
    return gen

def _pyscript(path, dest, executable, rsetup):
    gen = _zc_easy_install_pyscript(path, dest, executable, rsetup)
    try:
        os.chmod(dest, 0775)
    except (AttributeError, os.error):
        pass
    return gen

if 'OPENMDAO_REPO' in os.environ:
    zc.buildout.easy_install._script = _script
    zc.buildout.easy_install._pyscript = _pyscript
    
"""
new_sp_win = """
import os.path
prefx = os.path.join(sys.prefix,'Lib')
sys.path[2:] = [  prefx,
                 os.path.join(sys.prefix,'DLLs'),
               ]
import zc.buildout.buildout
"""

if sys.platform == 'win32':
    new_sp = new_sp_win
    bo_name = 'bin/buildout-script.py'
    # on windows, a buildout.exe file is generated that runs python
    # on the buildout-script.py file.  buildout.exe is really just
    # a copy of cli.exe from setuptools, which just looks for a file
    # named argv[0]+"-script.py" and runs python on it.
else:
    bo_name = 'bin/buildout'

f = open(bo_name,'r')
old = f.read()
f.close()

newf = open(bo_name,'w')
newf.write(old.replace(old_sp, new_sp))
newf.close()
if 'OPENMDAO_REPO' in os.environ:
    try:
        os.chmod(bo_name, 0775)
    except (AttributeError, os.error):
        pass
    
# now clean up the zc.buildout egg we installed 
# in the setup dir
for egg in fnmatch.filter(os.listdir(setupdir), boeggpat):
    if os.path.isdir(os.path.join(setupdir, egg)):
        shutil.rmtree(os.path.join(setupdir, egg))
