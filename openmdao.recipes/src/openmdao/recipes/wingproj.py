
import os
import os.path
import sys
import stat
import fnmatch
import ConfigParser
import logging
import pprint
import platform
from subprocess import Popen

import zc.buildout

from pkg_resources import working_set, get_entry_map
from pkg_resources import Environment, WorkingSet, Requirement, DistributionNotFound


def _find_files_and_dirs(pat, startdir):
    for path, dirlist, filelist in os.walk(startdir):
        for name in fnmatch.filter(filelist+dirlist, pat):
            yield os.path.join(path, name)
            

def _find_files(pat, startdir):
    for path, dirlist, filelist in os.walk(startdir):
        for name in fnmatch.filter(filelist, pat):
            yield os.path.join(path, name)

def _find_bzr(path=None):
    if not path:
        path = os.getcwd()
    if not os.path.exists(path):
        return None
    while path:
        if os.path.exists(os.path.join(path, '.bzr')):
            return path
        else:
            pth = path
            path = os.path.dirname(path)
            if path == pth:
                return None
    return None

def runwing(wingpath, projpath):
    # in order to find all of our shared libraries, find them
    # all and put their directories in LD_LIBRARY_PATH
    env = os.environ
    if platform.system() != 'Windows':
        libs = env.get('LD_LIBRARY_PATH','').split(os.pathsep)
        bzrtop = _find_bzr()
        if bzrtop:
            sodirs = set([os.path.dirname(x) for x in _find_files('*.so',bzrtop)])
            libs.extend(sodirs)
            env['LD_LIBRARY_PATH'] = os.pathsep.join(libs)
            
    Popen([wingpath, projpath], env=env)


_wing_header = """#!wing
#!version=3.0
##################################################################
# Wing IDE project file                                          #
##################################################################
"""

class WingProj(object):
    """Create a Wing IDE project file with python path set properly to be able
    to find files within a buildout. The user's default.wpr file will be used if
    present as the basis of the new project file.  After the buildout is run, 
    a file named wingproj.wpr will be created in the parts/wingproj directory of the 
    buildout.  This file will be updated during future buildouts only if the
    list of dependent eggs changes.
    """

    def __init__(self, buildout, name, options):
        self.buildout = buildout
        self.name = name
        self.options = options
        self.logger = logging.getLogger(name)
        self.branchdir = os.path.split(buildout['buildout']['directory'])[0]
        self.bindir = os.path.join(buildout['buildout']['directory'], 'bin')
        self.partsdir = buildout['buildout']['parts-directory']
        self.wingpath = options.get('wingpath', None)
        self.executable = buildout['buildout']['executable']
        
        dev_egg_dir = buildout['buildout']['develop-eggs-directory']
        dev_eggs = fnmatch.filter(os.listdir(dev_egg_dir),'*.egg-link')
        # grab the first line of each dev egg link file
        self.dev_eggs = [open(os.path.join(dev_egg_dir,f),'r').readlines()[0].strip() 
                            for f in dev_eggs]
        
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
            self.wingproj = os.path.join(os.path.dirname(__file__),
                                         'new_wing_proj.wpr')
        
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
                       dict({None: ('custom',os.pathsep.join(newset))}))
            config.set('user attributes', 'proj.pyexec', 
                       dict({None: ('custom', self.executable)}))

            if not config.has_option('project attributes', 'proj.directory-list'):
                
                excludes = [u'buildout', u'plans', 
                            u'docs/_build', u'docs/_static', u'docs/generated_images']
                # find all dirs containing setup.py files
                setupdirs = [os.path.dirname(p) for p in 
                                   _find_files_and_dirs('setup.py', self.branchdir)]
                for sdir in setupdirs:
                    sdir = sdir[len(self.branchdir):]
                    bname = os.path.basename(sdir)
                    excludes.append(unicode(os.path.join(sdir,
                                            '.'.join([bname,'egg-info'])).lstrip('/\\')))
                    excludes.append(unicode(os.path.join(sdir,'src',
                                            '.'.join([bname,'egg-info'])).lstrip('/\\')))
                    excludes.append(unicode(os.path.join(bname,'build').lstrip('/\\')))
                    
                config.set('project attributes', 'proj.directory-list',
                           pprint.pformat([{'dirloc': None,
                             'excludes': excludes,
                             'filter': '*.py ; *.rst',
                             'include_hidden': 0,
                             'recursive': 1,
                             'watch_for_changes': 1}]
                              ).replace('None',"loc('../../..')"))
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
        if self.wingpath:
            wingpath = self.wingpath
        else:
            if platform.system() == 'Windows':
                wingpath = 'wing'
            else:
                wingpath = 'wing3.1'
        #try:
        #    script = open(scriptname, 'w')
        #    script.write(script_template % dict(python=self.executable,
        #                                        proj=newfname,
        #                                        wingpath=wingpath))
        #    script.close()
        #except OSError, err:
        #    self.logger.error(str(err))
        #    raise zc.buildout.UserError('creation of wing script failed')
        #try:
        #    os.chmod(scriptname, 0775)
        #except (AttributeError, os.error):
        #    pass

        mydist = working_set.find(Requirement.parse('openmdao.recipes'))
        scripts = zc.buildout.easy_install.scripts(
            [('wing', 'openmdao.recipes.wingproj', 'runwing')], 
            WorkingSet([mydist.location]), 
            sys.executable, self.bindir, 
            arguments= "r'%s', r'%s'" % (wingpath, newfname))        
        
        return scripts

     
    update = install

