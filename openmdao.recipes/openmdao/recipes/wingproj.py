
import os
import os.path
import sys
import zc.buildout.easy_install


class WingProj(object):
    """Create a Wing IDE project file with paths set properly to be able
    to find files within a buildout.
    """

    def __init__(self, buildout, name, options):
        self.buildout = buildout
        self.name = name
        self.options = options
        self.branchdir = os.path.split(buildout['buildout']['directory'])[0]
        self.dev_eggs = buildout['buildout']['develop'].strip().splitlines()
        self.eggs = [os.path.join(buildout['buildout']['eggs-directory'],f)
                      for f in os.listdir(buildout['buildout']['eggs-directory'])]
        self.executable = buildout['buildout']['executable']
        self.wingproj = os.path.join(
                            self.branchdir,
                            'util','new_wing_proj.wpr')
        
    def install(self):
        
        all = self.dev_eggs + self.eggs
        npath = ':'.join(all)
        numsubs = len(npath)/52+1
        newpaths = [("'"+npath[i*52:i*52+51]+"'\\").rjust(76)
                                          for i in range(0,numsubs)]
        newpaths[0] = newpaths[0].strip()
        newpaths[len(newpaths)-1] = (22*' '+newpaths[len(newpaths)-1].strip()).rstrip('\\')
        
        wingfile = open(self.wingproj)
        contents = wingfile.read()
        wingfile.close()
        
        newcontents = contents.replace('XXX_PYTHON_EXE_XXX',
                                       self.executable)
        newcontents = newcontents.replace("'XXX_PYTHON_PATH_XXX'",
                                          '\n'.join(newpaths))
        newfile = open(os.path.join(self.branchdir,'wingproj.wpr'),'w')
        newfile.write(newcontents)
        newfile.close()
        
    update = install

