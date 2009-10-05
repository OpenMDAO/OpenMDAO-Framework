
import os
import os.path
import sys
import stat
import fnmatch
import ConfigParser
import logging
import pprint
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
    """Runs the Wing IDE after first setting environment variables
    necessary to locate shared libraries.
    """
    # in order to find all of our shared libraries, find them
    # all and put their directories in LD_LIBRARY_PATH
    env = os.environ
    if sys.platform != 'win32':
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
    
    The following options are supported:
    
    **eggs**
        The list of top level distributions that will be findable by Wing. Any
        distributions that are dependent upon the top level distributions will
        be determined automatically
    
    *wingpath*
        The path to the Wing executable
    
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
        self.specs = [r.strip() for r in options.get('eggs', '').split('\n')
                         if r.strip()]
        
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
        self.working_set = None
        
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
        # build up a list of all egg dependencies resulting from our 'eggs' parameter
        env = Environment(self.dev_eggs+[self.buildout['buildout']['eggs-directory'],
                                         os.path.join(self.buildout['buildout']['directory'],
                                                      'setup')])
        reqs = [Requirement.parse(x.strip()) for x in self.options['eggs'].split()]
        self.depdists = WorkingSet().resolve(reqs, env)
        self.working_set = WorkingSet([d.location for d in self.depdists])            

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
        newnames = self.dev_eggs+[d.location for d in self.depdists]
        newset = set(newnames)
        
        diff = oldset ^ newset
        
        if len(diff) > 0:    
               
            config.set('user attributes', 'proj.pypath', 
                       dict({None: ('custom',os.pathsep.join(newset))}))
            config.set('user attributes', 'proj.pyexec', 
                       dict({None: ('custom', self.executable)}))

            if not config.has_option('project attributes', 'proj.directory-list'):
                
                excludes = [u'buildout/bin', u'buildout/parts', u'buildout/develop-eggs',
                            u'buildout/eggs', u'buildout/setup', u'buildout/openmdao_log.txt',
                            u'plans', 
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
                             'filter': '*.py ; *.rst ; *.ini; *.cfg',
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
            if sys.platform == 'win32':
                wingpath = 'wing'
            else:
                wingpath = 'wing3.1'

        scripts = zc.buildout.easy_install.scripts(
            [('wing', 'openmdao.recipes.wingproj', 'runwing')], 
            self.working_set, 
            sys.executable, self.bindir, 
            arguments= "r'%s', r'%s'" % (wingpath, newfname))        
        
        return scripts


    update = install

