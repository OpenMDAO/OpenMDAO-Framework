
import os, sys
import zc.buildout.easy_install

# monkeypatch zc.buildout.easy_install to generate isolated scripts by
# replacing sys.path instead of prepending to it.  We only swap it out
# during install on the off chance that some other zc.buildout.egg recipe
# is being used as part of the buildout.

old_sp = 'import sys'
new_sp = """
import sys
import os.path
prefx = os.path.join(sys.prefix,'lib','python'+sys.version[0:3])
sys.path[:] = [  prefx+'.zip',
                 prefx,
                 os.path.join(prefx,'lib-dynload'),
                 os.path.join(prefx,'plat-'+sys.platform),
              ]
"""
new_sp_win = """
import sys
import os.path
prefx = os.path.join(sys.prefix,'Lib')
sys.path[:] = [  prefx,
                 os.path.join(sys.prefix,'DLLs'),
              ]
"""

if sys.platform == 'win32':
    new_sp = new_sp_win

_script_template = zc.buildout.easy_install.script_template.replace(old_sp,new_sp)
_py_script_template = zc.buildout.easy_install.py_script_template.replace(old_sp,new_sp)

import zc.recipe.egg

def _swap_templates(script, py_script):
    old_script = zc.buildout.easy_install.script_template
    old_py_script = zc.buildout.easy_install.py_script_template

    zc.buildout.easy_install.script_template = script
    zc.buildout.easy_install.py_script_template = py_script

    return (old_script, old_py_script)


import shutil

class IsolatedEgg(zc.recipe.egg.Scripts):
    """A recipe that modifies the zc.buildout.easy_install module script
    templates in order to isolate the buildout.  It changes the default
    behavior from (prepending to sys.path) to (replacing sys.path)."""

    def __init__(self, buildout, name, options):
        super(IsolatedEgg, self).__init__(buildout, name, options)
        self.buildout = buildout

    def install(self):
        old, old_py = _swap_templates(_script_template, _py_script_template)
        
        ret = []
        try:
            ret = super(IsolatedEgg, self).install()
        finally:
            _swap_templates(old, old_py)
                
        # Sometimes we need the explicit version command (eggsecutables).
        bin = self.buildout['buildout']['bin-directory']
        python = os.path.join(bin, 'python')
        pythonVR = os.path.join(bin, 'python'+sys.version[:3])
        shutil.copyfile(python, pythonVR)
        try:
            os.chmod(pythonVR, 0775)
        except (AttributeError, os.error):
            pass
        ret.append(pythonVR)
        return ret

    update = install

