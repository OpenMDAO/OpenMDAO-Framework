
import os
import os.path
import sys
import stat
import ConfigParser
from pkg_resources import working_set, get_entry_map
from pkg_resources import Environment, WorkingSet, Requirement, DistributionNotFound

script_template = """#!%(python)s

from subprocess import Popen
Popen(["wing3.1", "%(proj)s"])

"""

_wing_header = """#!wing
#!version=3.0
##################################################################
# Wing IDE project file                                          #
##################################################################
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
        self.executable = buildout['buildout']['executable']
        try:
            if sys.platform == 'win32':
                home = os.env['HOMEDRIVE']+os.env['HOMEPATH']
            else:
                home = os.env['HOME']
        except:
            home = ''
        
        self.wingproj = os.path.join(home, '.wingide3', 'default.wpr')    
        if not os.path.isfile(self.wingproj):
            self.wingproj = os.path.join(self.branchdir,
                                         'util','new_wing_proj.wpr')
        
        self.eggs = []
        env = Environment([buildout['buildout']['eggs-directory']])
        ws = WorkingSet()
        for entry,val in buildout.items():
            if 'eggs' in val:
                eggs = val['eggs'].split()
                for egg in eggs:
                    self._add_deps(self.eggs, env, ws, Requirement.parse(egg))

    def _add_deps(self, deps, env, ws, req):
        dist = env.best_match(req, ws)
        if dist is not None:
            deps.append(dist.location)
            reqs = dist.requires()
            for r in reqs:
                self._add_deps(deps, env, ws, r)
                        
    def format(self, names):
        npath = ':'.join(names)
        numsubs = len(npath)/53+1
        newpaths = [("'"+npath[i*53:i*53+53]+"'\\").rjust(70)
                                          for i in range(0,numsubs)]
        newpaths[0] = 14*' '+newpaths[0].strip()
        newpaths[len(newpaths)-1] = (14*' '+newpaths[len(newpaths)-1].strip()).rstrip('\\')+")}"
        newpaths[0:0] = ["{None: ('custom',"]
        return '\n'.join(newpaths)


    def unformat(self, namestr):
        path = namestr.split('\n')
        path = [p.strip("\\") for p in path]
        path = [p.strip("'") for p in path]
        path = ''.join(path)
        path = path.lstrip("{None: ('custom',")
        path = path.rstrip("\\n')}")
        return path.split(':') 

                
    def install(self):
        
        newfname = os.path.join(self.branchdir,'wingproj.wpr')
        if os.path.isfile(newfname):
            oldfile = newfname
        else:
            oldfile = self.wingproj
        
        config = ConfigParser.ConfigParser()
        config.read(oldfile)
        if config.has_option('user attributes', 'proj.pypath'):
            pypath = config.get('user attributes', 'proj.pypath')
            oldnames = self.unformat(pypath)
        else:
            oldnames = []
        
        oldset = set(oldnames)
        newnames = self.dev_eggs+self.eggs
        newset = set(newnames)
        
        diff = oldset ^ newset
        
        if len(diff) > 0:       
            newpaths = self.format(newnames)
            config.set('user attributes', 'proj.pypath', newpaths)
            config.set('user attributes', 'proj.pyexec', 
                       "{None: ('custom',\n"+18*" "+"'"+self.executable+"')}")
            config.set('project attributes', 'proj.directory-list',
                       "[{'dirloc': loc('.'),\n"+
                       "  'excludes': (),\n"+
                       "  'filter': '*.py',\n"+
                       "  'include_hidden': False,\n"+
                       "  'recursive': True,\n"+
                       "  'watch_for_changes': True}]")
            try:
                newfile = open(newfname, 'wb')
                newfile.write(_wing_header)
                config.write(newfile)
            finally:
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

