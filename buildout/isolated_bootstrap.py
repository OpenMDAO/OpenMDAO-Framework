"""Bootstrap a buildout-based project

This is a modified version of the bootstrap.py file (Copyright (c) 2006 
Zope Corporation and Contributors) that is
part of zc.buildout.  This version creates a bin/buildout script that is 
isolated from the system level installed packages. -BAN 6/25/09

Simply run this script in a directory containing a buildout.cfg.
The script accepts buildout command-line options, so you can
use the -c option to specify an alternate configuration file.

$Id$
"""

import os, shutil, sys, tempfile, urllib2

tmpeggs = tempfile.mkdtemp()

try:
    import pkg_resources
except ImportError:
    ez = {}
    exec urllib2.urlopen('http://peak.telecommunity.com/dist/ez_setup.py'
                         ).read() in ez
    ez['use_setuptools'](to_dir=tmpeggs, download_delay=0)

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

cmd = 'from setuptools.command.easy_install import main; main()'
ws  = pkg_resources.working_set
assert os.spawnle(
    os.P_WAIT, sys.executable, quote (sys.executable),
    '-c', quote (cmd), '-mqNxd', quote (tmpeggs), 'zc.buildout',
    dict(os.environ,
         PYTHONPATH=
         ws.find(pkg_resources.Requirement.parse('setuptools')).location
         ),
    ) == 0

ws.add_entry(tmpeggs)
ws.require('zc.buildout')
import zc.buildout.buildout

zc.buildout.buildout.main(sys.argv[1:] + ['bootstrap'])
shutil.rmtree(tmpeggs)

# now modify the bin/buildout script to isolate it

old_sp = 'import zc.buildout.buildout'
new_sp = """
import zc.buildout.buildout
import zc.buildout.easy_install
import os.path
import os

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
    
prefx = os.path.join(sys.prefix,'lib','python'+sys.version[0:3])
sys.path[:] = [  prefx+'.zip',
                 prefx,
                 os.path.join(prefx,'lib-dynload'),
                 os.path.join(prefx,'plat-'+sys.platform),
              ]+sys.path[0:2]
"""
new_sp_win = """
import zc.buildout.buildout
import os.path
prefx = os.path.join(sys.prefix,'Lib')
sys.path[:] = [  prefx,
                 os.path.join(sys.prefix,'DLLs'),
              ]+sys.path[0:2]
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
    
