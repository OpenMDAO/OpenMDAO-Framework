
import os
import os.path
import sys
import stat
import fnmatch
import ConfigParser
import logging

import zc.buildout

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

_LINE_WIDTH = 68

def _wingify(obj, left_margin=0):
    """Take an object, convert to a string, split it on commas. If any piece 
    is longer than 80 chars, split it up into smaller chunks. Finally, recombine 
    it all back into a string with each entry on a new line
    """
    flat = []
    parts = str(obj).strip().split(',')
    for idx, part in enumerate(parts):
        if len(part) < _LINE_WIDTH and idx < len(parts)-1:
            flat.append(part+',')
        else:
            part = part.strip()
            just = _LINE_WIDTH-left_margin
            numsubs = len(part)/just+1
            for i in range(0, numsubs):
                p = part[i*just:i*just+just]
                if not p.startswith("'"):
                    p = "'"+p
                p = left_margin*" "+p+"'\\"
                flat.append(p)
            flat[len(flat)-1] = flat[len(flat)-1][:-2]
    if " ')}" in flat[-1]:
        flat[-1] = flat[-1].replace(" ')}"," )}")
    return '\n'.join(flat)

    

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
        self.logger = logging.getLogger(name)
        self.branchdir = os.path.split(buildout['buildout']['directory'])[0]
        self.partsdir = buildout['buildout']['parts-directory']
        dev_egg_dir = buildout['buildout']['develop-eggs-directory']
        dev_eggs = fnmatch.filter(os.listdir(dev_egg_dir),'*.egg-link')
        # grab the first line of each dev egg link file
        self.dev_eggs = [open(os.path.join(dev_egg_dir,f),'r').readlines()[0].strip() 
                            for f in dev_eggs]
        self.executable = buildout['buildout']['executable']
        
        # try to find the default.wpr file in the user's home directory
        try:
            if sys.platform == 'win32':
                home = os.environ['HOMEDRIVE']+os.environ['HOMEPATH']
            else:
                home = os.environ['HOME']
        except:
            home = ''
        
        self.wingproj = os.path.join(home, '.wingide3', 'default.wpr')    
        if not os.path.isfile(self.wingproj):
            self.wingproj = os.path.join(self.branchdir,
                                         'misc','new_wing_proj.wpr')
        
        # build up a list of all egg dependencies we find in other recipes in this
        # buildout. We just look for the keyword 'eggs' and look into the eggs
        # directory for matching distributions
        self.eggs = []
        env = Environment(self.dev_eggs+[buildout['buildout']['eggs-directory']])
        ws = WorkingSet()
        for entry,val in buildout.items():
            if 'eggs' in val:
                eggs = [x.strip() for x in val['eggs'].split()]
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
                        
    def _unformat(self, namestr):
        """Take a path string from the Wing project file and chop it up into 
        individual paths.
        """
        path = namestr.split('\n')
        path = [p.strip("\\").strip("'") for p in path]
        path = ''.join(path)
        path = path.lstrip("{None: ('custom',")
        path = path.rstrip("\\n')}")
        return path.split(':') 

                
    def install(self):
        
        if not os.path.isdir(os.path.join(self.partsdir,'wingproj')):
            os.makedirs(os.path.join(self.partsdir,'wingproj'))
            
        newfname = os.path.join(self.partsdir, 'wingproj','wingproj.wpr')
        if os.path.isfile(newfname):
            oldfile = newfname
        else:
            oldfile = self.wingproj
        
        self.logger.info('reading wing config from %s' % oldfile)
        
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
               
            config.set('user attributes', 'proj.pypath', 
                       _wingify(dict({None: ('custom',':'.join(newset))}), left_margin=18))
            config.set('user attributes', 'proj.pyexec', 
                       _wingify(dict({None: ('custom', self.executable)}), left_margin=18))

            if not config.has_option('project attributes', 'proj.directory-list'):
                config.set('project attributes', 'proj.directory-list',
                            "[{'dirloc': loc('../../..'),\n"+
                            "  'excludes': [u'openmdao.util/build',\n"+
                                     "u'openmdao.lib/src/openmdao.lib.egg-info',\n"+
                                     "u'buildout',\n"+
                                     "u'plans',\n"+
                                     "u'openmdao.test/src/openmdao.test.egg-info',\n"+
                                     "u'docs',\n"+
                                     "u'openmdao.lib/build',\n"+
                                     "u'eggsrc/conmin/conmin.egg-info',\n"+
                                     "u'openmdao.main/build',\n"+
                                     "u'openmdao.recipes/build',\n"+
                                     "u'openmdao.recipes/src/openmdao.recipes.egg-info',\n"+
                                     "u'openmdao.test/build',\n"+
                                     "u'openmdao.main/src/openmdao.main.egg-info',\n"+
                                     "u'openmdao.util/src/openmdao.util.egg-info',\n"+
                                     "u'eggsrc/npsscomponent/build'],\n"+
                            "  'filter': '*.py',\n"+
                            "  'include_hidden': 0,\n"+
                            "  'recursive': 1,\n"+
                            "  'watch_for_changes': 1}]")
            try:
                self.logger.info('egg set has changed - writing config to %s' %
                                  newfname)
                newfile = open(newfname, 'wb')
                newfile.write(_wing_header)
                config.write(newfile)
            except Exception, err:
                self.logger.error(str(err))
                raise zc.buildout.UserError('write of config failed')
            finally:
                newfile.close()
        
        # create a bin/wing script
        scriptname = os.path.join(self.buildout['buildout']['directory'],
                                  'bin','wing')
        try:
            script = open(scriptname, 'w')
            script.write(script_template % dict(python=self.executable,
                                                proj=newfname))
            script.close()
            os.chmod(scriptname, 0755)
        except OSError, err:
            self.logger.error(str(err))
            raise zc.buildout.UserError('creation of wing script failed')
        return [scriptname]

     
    update = install

