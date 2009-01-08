
import os
import os.path
import sys
import zc.buildout.easy_install


class WingProj(object):
    """Create a Wing IDE project file with python path set properly to be able
    to find files within a buildout.
    
    TODO: possibly add automatic inclusion of all OpenMDAO source files
          to the project. This may not always be what a developer wants though...
    """

    def __init__(self, buildout, name, options):
        self.buildout = buildout
        self.name = name
        self.options = options
        self.branchdir = os.path.split(buildout['buildout']['directory'])[0]
        dev_eggs = buildout['buildout']['develop'].strip().splitlines()
        self.dev_eggs = [os.path.abspath(f) for f in dev_eggs]
        self.eggs = [os.path.join(buildout['buildout']['eggs-directory'],f)
                      for f in os.listdir(buildout['buildout']['eggs-directory'])]
        self.executable = buildout['buildout']['executable']
        self.wingproj = os.path.join(
                            self.branchdir,
                            'util','new_wing_proj.wpr')
        
    def install(self):
        
        all = self.dev_eggs + self.eggs
        npath = ':'.join(all)
        numsubs = len(npath)/57+1
        newpaths = [("'"+npath[i*57:i*57+57]+"'\\").rjust(81)
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
        newfname = os.path.join(self.branchdir,'wingproj.wpr')
        newfile = open(newfname,'w')
        newfile.write(newcontents)
        newfile.close()
        
        return [newfname]
        
    update = install

