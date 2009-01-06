
import os, sys
import zc.buildout.easy_install

# monkeypath zc.buildout.easy_install to generate isolated scripts by
# replacing sys.path instead of prepending to it.  
# TODO: This is a bit fragile, so make it more robust at some point

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

zc.buildout.easy_install.script_template = zc.buildout.easy_install.script_template.replace(old_sp,new_sp)
zc.buildout.easy_install.py_script_template = zc.buildout.easy_install.py_script_template.replace(old_sp,new_sp)

import zc.recipe.egg

class IsolatedEgg(zc.recipe.egg.Scripts):
    """A dummy recipe that just dumps out what is passed to it"""

"""
    def __init__(self, buildout, name, options):
        print 'buildout = ',buildout
        print 'name = ',name
        print 'options = ',options

    def install(self):
        return []

    update = install
"""

