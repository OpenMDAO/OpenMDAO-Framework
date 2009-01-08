
import os
import os.path
import sys
import stat
import zc.buildout.easy_install

script_template = """#!%(python)s

from subprocess import Popen
Popen(["wing3.1", "%(proj)s"])

"""

class WingProj(object):
    """Create a Wing IDE project file with python path set properly to be able
    to find files within a buildout.
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
        newpaths[0] = "                     "+newpaths[0].strip()
        newpaths[len(newpaths)-1] = (22*' '+newpaths[len(newpaths)-1].strip()).rstrip('\\')+")}"
        newpaths[0:0] = ["proj.pypath = {None: ('custom',"]

        newfname = os.path.join(self.branchdir,'wingproj.wpr')
        
        if os.path.isfile(newfname):
            oldfile = open(newfname,'r')
        else:
            oldfile = open(self.wingproj,'r')
            
        lines = oldfile.readlines()
        oldfile.close()

        inpath = False
        for i,line in enumerate(lines):
            if inpath is True:
                if '}' in line:
                    path_end = i+1
                    inpath = False
            elif line.startswith('proj.pyexec'):
                exec_start = i
                exec_end = i+2
            elif line.startswith('proj.pypath'):
                path_start = i
                inpath = True
                
        lines[exec_start:exec_end] = ["proj.pyexec = {None: ('custom',",
                                      "                      '"+
                                      self.executable+"')}"]
        lines[path_start:path_end] = newpaths
        
        newfile = open(newfname, 'w')
        newfile.write('\n'.join(lines))
        newfile.close()
        
        # create a bin/wing script
        scriptname = os.path.join(self.buildout['buildout']['directory'],
                                  'bin','wing')
        script = open(scriptname, 'w')
        script.write(script_template % dict(python=self.executable,
                                            proj=newfname))
        script.close()
        os.chmod(scriptname, 
                 stat.S_IREAD|stat.S_IWRITE|stat.S_IEXEC|
                 os.stat(scriptname).st_mode)
        
        return []
        
    update = install

