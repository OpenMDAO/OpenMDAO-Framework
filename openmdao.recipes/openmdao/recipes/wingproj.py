
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

def wingify(obj):
    """Take an object, convert to a string, split it on commas. If any piece 
    is longer than 80 chars, split it up into smaller chunks. Finally, recombine 
    it all back into a string with each entry on a new line
    """
    flat = []
    parts = str(obj).split(',')
    for part in parts:
        if len(part) < 80:
            flat.append(part)
        else:
            pass
            
    

class WingProj(object):
    """Create a Wing IDE project file with python path set properly to be able
    to find files within a buildout. The user's default.wpr file will be used if
    present as the basis of the new project file.  After the buildout is run, 
    a file named wingproj.wpr will be created in the parent directory of the 
    buildout.  This file will be updated during future buildouts only if the
    list of dependent eggs changes.
    """

    def __init__(self, buildout, name, options):
        self.buildout = buildout
        self.name = name
        self.options = options
        self.branchdir = os.path.split(buildout['buildout']['directory'])[0]
        dev_eggs = buildout['buildout']['develop'].strip().splitlines()
        self.dev_eggs = [os.path.abspath(f) for f in dev_eggs]
        self.executable = buildout['buildout']['executable']
        
        # try to find the default.wpr file in the user's home directory
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
        
        # build up a list of all egg dependencies we find in other recipes in this
        # buildout. We just look for the keyword 'eggs' and look into the eggs
        # directory for matching distributions
        self.eggs = []
        env = Environment([buildout['buildout']['eggs-directory']].extend(self.dev_eggs))
        ws = WorkingSet()
        for entry,val in buildout.items():
            if 'eggs' in val:
                eggs = val['eggs'].split()
                for egg in eggs:
                    self._add_deps(self.eggs, env, ws, Requirement.parse(egg))

    def _add_deps(self, deps, env, ws, req):
        """Add a dependency for the given requirement and anything the resulting
        distrib depends on.
        """
        dist = env.best_match(req, ws)
        if dist is not None:
            deps.append(dist.location)
            reqs = dist.requires()
            for r in reqs:
                self._add_deps(deps, env, ws, r)
                        
    def _format(self, names):
        """Take a list of paths to distribs and format them the way Wing wants them.
        """
        npath = ':'.join(names)
        numsubs = len(npath)/53+1
        newpaths = [("'"+npath[i*53:i*53+53]+"'\\").rjust(70)
                                          for i in range(0,numsubs)]
        newpaths[0] = 14*' '+newpaths[0].strip()
        newpaths[len(newpaths)-1] = (14*' '+newpaths[len(newpaths)-1].strip()).rstrip('\\')+")}"
        newpaths[0:0] = ["{None: ('custom',"]
        return '\n'.join(newpaths)


    def _unformat(self, namestr):
        """Take a path string from the Wing project file and chop it up into 
        individual paths.
        """
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
            oldnames = self._unformat(pypath)
        else:
            oldnames = []
        
        oldset = set(oldnames)
        newnames = self.dev_eggs+self.eggs
        newset = set(newnames)
        
        diff = oldset ^ newset
        
        if len(diff) > 0:       
            newpaths = self._format(newnames)
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

