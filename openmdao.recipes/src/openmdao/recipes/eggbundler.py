
import os
import sys
import os.path
import shutil
from pkg_resources import Environment, WorkingSet, Requirement
from setuptools.package_index import PackageIndex
import tarfile
import logging
import zc.buildout

from openmdao.util.procutil import run_command
from openmdao.util.fileutil import rm


    
class EggBundler(object):
    """Collect all of the eggs (not installed) that are used in the current
    buildout and put them along with a custom buildout config into a 
    gzipped tar file.  The tar file should provide a totally self-contained
    buildout environment (after bootstrapping the buildout). If the
    'pin_versions' variable is set to true in the buildout, the bundle
    buildout config will be hardwired to specific versions for all dependencies.
    """

    def __init__(self, buildout, name, options):
        self.name = name
        self.buildout = buildout
        self.options = options
        self.logger = logging.getLogger(name)
        self.executable = buildout['buildout']['executable']        
        self.bodir = buildout['buildout']['directory']
        self.branchdir = os.path.dirname(self.bodir)
        self.egg_dir = buildout['buildout']['eggs-directory']
        self.partsdir = buildout['buildout']['parts-directory']
        self.develop = [x.strip() for x in buildout['buildout']['develop'].split('\n') 
                                     if x.strip()]
        self.bundledir = os.path.join(self.partsdir, name)
        bundle_cache = os.path.join(self.bundledir,'buildout',
                                    'distrib-cache','dist')
        self.bundle_cache = bundle_cache
        self.env = Environment([bundle_cache])
        self.installed_env = Environment([self.egg_dir])
        self.index = PackageIndex(buildout['buildout']['index'])
        self.bundle_name = options['bundle_name']
        self.bundle_version = options['bundle_version']
        self.configs = options.get('buildout_configs') or ['buildout.cfg']
        fix = options.get('pin_versions').strip().lower()
        if fix == 'false':
            self.pin_versions = False
        else:
            self.pin_versions = True
        self.readme = options.get('readme','').strip()
        if not self.readme:
            self.logger.warning('No README file has been supplied to this recipe')
        
        self.excludeparts = [x for x in options['exclude_parts'].split() if x != '']
        self.parts = [x for x in buildout['buildout']['parts'].split() 
                               if x != '' and x not in self.excludeparts]
        self.dists = []
        archive = options.get('archive').strip().lower()
        if archive == 'false':
            self.archive = False
        else:
            self.archive = True
            
        extra_stuff = [x.strip() for x in options.get('extra_data', '').split('\n') if x.strip()]
        self.extra_stuff = []
        for stuff in extra_stuff:
            try:
                src,dest = stuff.split('=')
            except ValueError:
                src = stuff
                dest = os.path.basename(stuff)
            if os.path.isabs(dest):
                raise zc.buildout.UserError(
                     'absolute path %s is illegal as a destination name' %
                      dest)
                
            self.extra_stuff.append((src,dest))

    def _add_deps(self, deps, env, ws, req, excludes):
        """Add a dependency for the given requirement and anything the resulting
        distrib depends on.
        """
        self.logger.debug('looking for %s',req)
        
        if req.project_name in excludes:
            return
        dist = env.best_match(req, ws)

        if dist in deps:
            return
                        
        if dist is None:
            fetched = self.index.fetch_distribution(req,self.bundle_cache)
            if fetched is None:
                self.logger.debug('could not find distrib for %s' % req)
        else:
            self.logger.debug('found dist %s' % dist.as_requirement())
            if dist.project_name not in excludes:
                deps.add(dist)
            for req in dist.requires():
                self.logger.debug('%s required by %s' % (req, dist.project_name))
                self._add_deps(deps, env, ws, req, excludes)


    def _create_buildout_dir(self):
        """Creates a bootstrappable buildout dir with a buildout.cfg file
        tailored to build from a download-cache.
        """ 
        bodir = os.path.join(self.bundledir, 'buildout')  
        shutil.copy(os.path.join(self.bodir, 'isolated_bootstrap.py'), bodir)
        
        # now create the buildout.cfg file
        bo = self.buildout
        
        boexcludes = set(['recipe', 'bin-directory', 'executable',
                          'eggs-directory', 'develop-eggs-directory',
                          '_e', '_d', '_b', '__buildout_signature__', 'index'])
        f = open(os.path.join(bodir, 'buildout.cfg'), 'w')
        for sect,opts in bo.items():
            if sect == 'buildout':
                versions = self.buildout['buildout'].get('versions') or \
                           (self.name+'_release')
                self.excludeparts.append(versions)
                f.write('[buildout]\n\n')
                f.write('newest = %s\n\n' % self.buildout['buildout']['newest'])
                f.write('offline = %s\n\n' % self.buildout['buildout']['offline'])
                #f.write('index = %s\n\n' % self.buildout['buildout']['index'])
                f.write('parts = \n')
                for part in self.parts:
                    f.write('   %s\n' % part)
                f.write('\n\ndownload-cache = distrib-cache\n\n')
                f.write('\n\n')
                if self.pin_versions:
                    f.write('versions = %s\n\n' % versions)
                    f.write('[%s]\n' % versions)
                    projs = ['%s = %s' % (x.project_name, x.version) 
                          for x in self.dists if x.project_name != 'setuptools']
                    self.logger.debug('fixed version list:')
                    for proj in sorted(set(projs)):
                        self.logger.debug(proj)
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
                            f.write('%s = %s\n\n' % (opt, val))
                    

    def _build_dev_eggs(self, startdir):
        """ Loop through all of the develop eggs and build real eggs to
        put into the download-cache.
        """
        env = Environment([self.bundle_cache])
        for degg in self.develop:
            os.chdir(startdir)
            self.logger.info('building egg in %s' % degg)
            os.chdir(degg)
            if os.path.exists('build'):
                shutil.rmtree('build')
            
            cmd = '%s setup.py build bdist_egg -d %s' % (self.executable, 
                                                         self.bundle_cache)
            out, ret = run_command(cmd)
            if ret != 0:
                self.logger.error(out)
                raise zc.buildout.UserError(
                     'error while building egg in %s (return code=%d)' 
                      % (degg,ret))
    
                        
    def _tarfile_name(self):
        """ Returns name of tar file to be created. """
        return os.path.join(self.bundledir, '%s-bundle-%s-py%s.tar.gz' % 
                                            (self.bundle_name,
                                             self.bundle_version,
                                             sys.version[:3]))

    def install(self):
        distribs = set()
        ws = WorkingSet()
        startdir = os.getcwd()
        if os.path.exists(self.bundledir):
            self.logger.info('removing old bundle directory %s' % self.bundledir)
            shutil.rmtree(self.bundledir)
            
        if os.path.exists(self.bundle_cache):
            shutil.rmtree(self.bundle_cache)
            
        os.makedirs(self.bundle_cache)
        
        try:
            self._build_dev_eggs(startdir)
        finally:
            os.chdir(startdir)
        
        # collect dependencies for all develop eggs 
        tmpenv = Environment([self.buildout['buildout']['develop-eggs-directory']])
        devprojs = [x for x in tmpenv]  # list of dev project names
        for dproj in devprojs:
            for dist in tmpenv[dproj]:
                self.logger.debug('adding deps for dev egg %s' % dist.location)
                self.dists.append(dist)
                for req in dist.requires():
                    self.logger.debug('%s requires %s' % \
                                      (dist.project_name, req))
                    # use the installed environment to gather dependencies
                    # because retrieving them from uninstalled ditros doesn't
                    # work in all cases
                    self._add_deps(distribs, self.installed_env, 
                                   ws, req, devprojs)
        
        # build up a list of all egg dependencies as specified in the 'eggs' attribute
        for spec in [x.strip() for x in self.options['eggs'].split('\n') if x.strip()]:
            self._add_deps(distribs, self.installed_env, ws, 
                           Requirement.parse(spec), devprojs)
        
        # get the total set of all distribs (including develop eggs & all deps)
        self.dists.extend(distribs)
        
        # Copy any extra stuff specified in the config
        for src, dest in self.extra_stuff:
            dest = os.path.join(self.bundledir, dest)
            self.logger.debug('copying %s to %s' % (src, dest))
            if not os.path.isdir(src):
                dname = os.path.dirname(dest)
                if not os.path.exists(dname): 
                    os.makedirs(dname)
                
            if os.path.isfile(src):
                shutil.copy(src, dest)
            elif os.path.isdir(src):
                if os.path.exists(dest):
                    rm(dest)
                shutil.copytree(src, dest) 
            else:
                self.logger.error('%s is not a file or directory' % src)
            
        # Copy all of the dependent distribs into the cache directory.
        # The eggs made from the develop eggs are already there.
        for dist in distribs:
            self.logger.debug('fetching %s from index' % 
                              dist.project_name)
            fetched = self.index.fetch_distribution(dist.as_requirement(), 
                                                    self.bundle_cache)
            if fetched is None:
                self.logger.debug('could not find %s' % dist.project_name)
        
        self._create_buildout_dir()                                  
        
        if self.readme:
            shutil.copy(self.readme, os.path.join(self.bundledir, 'README.txt'))

        if self.archive is True:
            tarname =  self._tarfile_name()
            self.logger.info('creating tar file %s' % tarname)
           
            tarf = tarfile.open(tarname, mode='w:gz')
            tarf.add(self.bundledir, arcname='%s-%s-py%s' %
                                    (self.bundle_name, self.bundle_version,
                                     sys.version[:3]))
            tarf.close()
        
            # delete everything but the tar file
            for name in os.listdir(self.bundledir):
                pname = os.path.join(self.bundledir, name)
                if pname != tarname:
                    try:
                        rm(pname)
                    except OSError, err:
                        self.logger.error(str(err))
            
        return [self.bundledir]


    update = install

