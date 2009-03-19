
import os
import sys
import os.path
import shutil
from pkg_resources import Environment, WorkingSet, Requirement
from setuptools.package_index import PackageIndex
import tarfile
import logging
import ConfigParser
import copy
from zc.buildout.buildout import _open as open_buildout_cfg

from openmdao.util.procutil import run_command
from openmdao.util.fileutil import rm
    
class EggBundler(object):
    """Collect all of the eggs (not installed) that are used in the current
    buildout, and put them along with a custom buildout config into a 
    gzipped tar file.  The tar file should provide a totally self-contained
    buildout environment (after bootstrapping the buildout). If the
    'fix_versions' variable is set to true in the buildout, the bundle
    buildout config will be hardwired to specific versions for all dependencies.
    """

    def __init__(self, buildout, name, options):
        self.buildout = buildout
        self.name = name
        self.options = options
        self.logger = logging.getLogger(name)
        self.bodir = buildout['buildout']['directory']
        self.branchdir = os.path.split(self.bodir)[0]
        self.interpreter = os.path.join(buildout['buildout']['bin-directory'], 'python')
        self.executable = buildout['buildout']['executable']        
        self.egg_dir = buildout['buildout']['eggs-directory']
        self.develop = [x for x in buildout['buildout']['develop'].split() if x != '']
        self.partsdir = buildout['buildout']['parts-directory']
        self.downloads = os.path.join(buildout['buildout']['download-cache'],
                                      'dist')
        self.bundledir = os.path.join(self.partsdir, name)
        bundle_cache = os.path.join(self.bundledir,'buildout',
                                    'distrib-cache','dist')
        self.bundle_cache = bundle_cache
        self.env = Environment([bundle_cache, self.downloads])
        self.index = PackageIndex(buildout['buildout']['index'])
        self.bundle_name = options['bundle_name']
        self.bundle_version = options['bundle_version']
        self.configs = options.get('buildout_configs') or ['buildout.cfg']
        self.fix_versions = bool(options.get('fix_versions') or True)
        self.extra_stuff = (options.get('extra_data') or '').split()
        
        self.excludeparts = [x for x in options['exclude_parts'].split() if x != '']
        self.parts = [x for x in buildout['buildout']['parts'].split() 
                               if x != '' and x not in self.excludeparts]
        self.dists = []

    def _add_deps(self, deps, env, ws, req, excludes):
        """Add a dependency for the given requirement and anything the resulting
        distrib depends on.
        """
        if req.project_name in excludes:
            return
        dist = env.best_match(req, ws)
        if dist is not None:
            if dist.project_name not in excludes:
                deps.add(dist)
            reqs = dist.requires()
            for r in reqs:
                self._add_deps(deps, env, ws, r, excludes)
        else:
            dist = self.index.fetch_distribution(req, self.downloads)
            if dist is None:
                self.logger.error('Cannot find distrib for "%s"' % req)
            else:
                if dist.project_name not in excludes:
                    deps.add(dist)

    def _create_buildout_dir(self): 
        bodir = os.path.join(self.bundledir, 'buildout')  
        #if not os.path.isdir(bodir):            
        #    os.mkdir(bodir)
        shutil.copy(os.path.join(self.bodir, 'isolated_bootstrap.py'),
                    os.path.join(bodir,'isolated_bootstrap.py'))
        
        # now create the buildout.cfg file
        bo = self.buildout
        
        boexcludes = set(['recipe','bin-directory','executable','eggs-directory',
                          'develop-eggs-directory','_e','_d','_b',
                          '__buildout_signature__','index'])
        f = open(os.path.join(bodir,'buildout.cfg'),'w')
        for sect,opts in bo.items():
            if sect=='buildout':
                f.write('[buildout]\n\n')
                f.write('newest = %s\n\n' % self.buildout['buildout']['newest'])
                f.write('offline = %s\n\n' % self.buildout['buildout']['offline'])
                f.write('parts = \n')
                for part in self.parts:
                    f.write('   %s\n' % part)
                f.write('\n\ndownload-cache = distrib-cache\n\n')
                f.write('\n\n')
                if self.fix_versions:
                    f.write('versions = %s\n\n' % (self.name+'_release'))
                    f.write('[%s]\n' % (self.name+'_release'))
                    projs = ['%s = %s'%(x.project_name,x.version) 
                                                      for x in self.dists]
                    for proj in sorted(set(projs)):
                        f.write('%s\n' % proj)
                        
                f.write('\n\n')
            elif sect not in self.excludeparts:
                f.write('\n[%s]\n' % sect)
                f.write('recipe = %s\n\n' % opts['recipe'])
                for opt, val in opts.items():
                    if opt not in boexcludes:
                        if '\n' in val:
                            f.write('%s = ' % opt)
                            for line in val.splitlines():
                                if line.strip() != '':
                                    f.write('  %s\n' % line)
                        else:
                            f.write('%s = %s\n\n' % (opt,val))
                    
                    
    def install(self):
        distribs = set()
        ws = WorkingSet()
        startdir = os.getcwd()
        
        if not os.path.isdir(self.bundle_cache):
            os.makedirs(self.bundle_cache)
        
        # loop through all of the develop eggs and build real eggs to
        # put into the download-cache
        try:
            for degg in self.develop:
                self.logger.info('building egg in %s' % degg)
                os.chdir(degg)
                cmd = '%s setup.py bdist_egg -d %s' % (self.executable, 
                                                       self.bundle_cache)
                out, ret = run_command(cmd)
                if ret != 0:
                    self.logger.error(out)
                    raise zc.buildout.UserError(
                         'error while building egg in %s (return code=%d)' 
                          % (degg,ret))
        finally:
            os.chdir(startdir)
        
        # collect dependencies for all develop eggs 
        tmpenv = Environment([self.bundle_cache])
        excludes = [x for x in tmpenv]
        for ex in excludes:
            for dist in tmpenv[ex]:
                self.dists.append(dist)
                for r in dist.requires():
                    self._add_deps(distribs, self.env, ws, r, excludes)
        
        # build up a list of all egg dependencies we find in other recipes in
        # this buildout.
        for dl in [x for x in self.options['distrib_lists'].split() if x != '']:
            part, attrib = dl.split('.')
            for spec in [x for x in self.buildout[part][attrib].split() 
                                                                if x != '']:
                self._add_deps(distribs, self.env, ws, 
                               Requirement.parse(spec), excludes)
        
        # get the total set of all distribs (including develop eggs & all deps)
        self.dists.extend(distribs)
        
        # Copy any extra stuff specified in the config
        for stuff in self.extra_stuff:
            if os.path.isfile(stuff):
                shutil.copy(stuff, self.bundledir)
            elif os.path.isdir(stuff):
                dest = os.path.join(self.bundledir,os.path.basename(stuff))
                if os.path.exists(dest):
                    rm(dest)
                shutil.copytree(stuff, dest) 
            
        # Copy all of the dependent distribs into the cache directory.
        # The eggs made from the develop eggs are already there.
        self.logger.info('copying eggs')
        for dist in distribs:
            shutil.copy(dist.location, os.path.join(self.bundle_cache,
                                             os.path.basename(dist.location)))
        
        self.logger.info('creating buildout config')
        self._create_buildout_dir()                                  
        
        tarname = os.path.join(self.bundledir,'%s-bundle-%s-py%s.tar.gz' % 
                                   (self.bundle_name,self.bundle_version,
                                    sys.version[:3]))
        self.logger.info('creating tar file %s' %  tarname)
           
        tarf = tarfile.open(tarname, mode='w:gz')
        tarf.add(self.bundledir,arcname='%s-%s-py%s' %
                                   (self.bundle_name,self.bundle_version,
                                    sys.version[:3]))
        tarf.close()
        
        #rm(os.path.join(self.bundledir,'buildout',))
            
        return [self.bundledir]

   
    def update(self):
        return []             

