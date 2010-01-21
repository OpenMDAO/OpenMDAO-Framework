
import os
import sys
import shutil
from pkg_resources import Environment, WorkingSet, Requirement, Distribution, DEVELOP_DIST
from setuptools.package_index import PackageIndex
import tarfile
import logging
import fnmatch
import urllib2

import zc.buildout
import setuptools

from subprocess import Popen, PIPE, STDOUT

def _find_files(startdir):
    for path, dirlist, filelist in os.walk(startdir):
        for name in filelist:
            yield os.path.join(path, name)
            
            
def rm(path):
    """Delete a file or directory"""
    if os.path.isdir(path):
        shutil.rmtree(path)
    else:
        os.remove(path)

class Bundler(object):
    """Collect all of the eggs that are used in the current
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
        self.develop = [x.strip() for x in buildout['buildout']['develop'].split('\n') 
                                     if x.strip()]
        
        self.bundledir = os.path.join(buildout['buildout']['parts-directory'], name)
        self.bundle_cache = os.path.join(self.bundledir,'buildout',
                                    'distrib-cache','dist')
        
        self.bundle_version = options['bundle_version']
        self.configs = options.get('buildout_configs') or ['buildout.cfg']
        fix = options.get('pin_versions').strip().lower()
        if fix == 'false':
            self.pin_versions = False
        else:
            self.pin_versions = True
        self.readme = options.get('readme','').strip()
        if not self.readme:
            self.logger.warning('No README file has been supplied to this recipe!')
        
        self.excludeparts = [x for x in options['exclude_parts'].split() if x != '']
        self.parts = [x for x in buildout['buildout']['parts'].split() 
                               if x != '' and x not in self.excludeparts]

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


    def _setup_buildout_dir(self):
        """Creates a bootstrappable buildout dir with a buildout.cfg file
        tailored to build from a download-cache.
        """ 
        bodir = os.path.join(self.bundledir, 'buildout')  
        shutil.copy(os.path.join(self.buildout['buildout']['directory'], 
                                 'isolated_bootstrap.py'), bodir)
        
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
                f.write('eggs-directory = ${buildout:directory}/eggs\n\n')
                f.write('parts = \n')
                for part in self.parts:
                    f.write('   %s\n' % part)
                f.write('\n\ndownload-cache = distrib-cache\n\n')
                f.write('\n\ninstall-from-cache = true\n\n')
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
        """Build real eggs out of all of the develop eggs."""
        for degg in self.develop:
            os.chdir(startdir)
            absegg = os.path.abspath(degg)
            self.logger.info('building egg in %s' % degg)
            os.chdir(degg)
            # clean up any old builds
            if os.path.exists('build'):
                shutil.rmtree('build')
            
            cmd = '%s setup.py build bdist_egg -d %s' % (self.buildout['buildout']['executable'], 
                                                         self.bundle_cache)
            p = Popen(cmd, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
            out = p.communicate()[0]
            ret = p.returncode
            if ret != 0:
                self.logger.error(out)
                raise zc.buildout.UserError(
                     'error while building egg in %s (return code=%d)' 
                      % (degg,ret))
            
            matches = fnmatch.filter(os.listdir(self.bundle_cache), 
                                     os.path.basename(absegg)+'-*')
            if len(matches) != 1:
                raise RuntimeError("Must have a single match for distrib %s in cache but found %s" %
                                  (dist.project_name, matches))
            dist = Distribution.from_filename(matches[0])
            if dist.platform is None:  # pure python distrib, so we can 'pre-install' it in eggs dir
                self.logger.debug("unpacking archive %s to %s" %
                                  (os.path.join(self.bundle_cache,matches[0]),
                                   os.path.join(self.bundledir,'buildout','eggs',
                                                matches[0])))
                setuptools.archive_util.unpack_archive(os.path.join(self.bundle_cache,
                                                                    matches[0]),
                                                       os.path.join(self.bundledir,
                                                                    'buildout','eggs',
                                                                    matches[0]))
                # get rid of the zipped egg we built
                os.remove(os.path.join(self.bundle_cache, matches[0]))
    
                        
    def _tarfile_name(self):
        """ Returns name of tar file to be created. """
        return os.path.join(self.bundledir, '%s-bundle-%s-py%s.tar.gz' % 
                                            (self.options['bundle_name'],
                                             self.bundle_version,
                                             sys.version[:3]))

    def install(self):
        startdir = os.getcwd()
        if os.path.exists(self.bundledir):
            self.logger.debug('removing old bundle directory %s' % self.bundledir)
            shutil.rmtree(self.bundledir)
            
        if os.path.exists(self.bundle_cache):
            shutil.rmtree(self.bundle_cache)
            
        os.makedirs(os.path.join(self.bundledir, 'buildout', 'eggs'))            
        os.makedirs(self.bundle_cache)
        
        try:
            self._build_dev_eggs(startdir)
        finally:
            os.chdir(startdir)
            
        eggdir = os.path.join(self.bundledir, 'buildout', 'eggs')
        env = Environment([self.buildout['buildout']['eggs-directory'], 
                           eggdir,
                           os.path.join(self.buildout['buildout']['directory'],
                                        'setup'),
                           self.buildout['buildout']['develop-eggs-directory']])
        reqs = [Requirement.parse(x.strip()) for x in self.options['eggs'].split()]
        self.dists = WorkingSet().resolve(reqs, env)        
        
        self._setup_buildout_dir()  
        
        # Check that we can contact the egg server.
        try:
            url = self.buildout['buildout']['index']
        except KeyError:
            url = 'http://pypi.python.org/pypi'
        try:
            spath = self.buildout['buildout']['extra-paths']
            search_path = [x.strip() for x in spath.split('\n') if x.strip()]
        except KeyError:
            search_path = None
        self.logger.info('using URL %s', url)
        self.logger.info('    and search path %s', search_path)

        if self.dists:
            proxy_support = urllib2.ProxyHandler({})
            opener = urllib2.build_opener(proxy_support)
            try:
                opener.open(url).read()
            except urllib2.URLError, exc:
                msg = "Can't contact egg server at '%s': %s" % (url, exc)
                raise zc.buildout.UserError(msg)

        index = PackageIndex(url, search_path=search_path)
        try:
            flinks = self.buildout['buildout']['find-links']
        except KeyError:
            pass
        else:
            findlinks = [x.strip() for x in flinks.split('\n') if x.strip()]
            index.add_find_links(findlinks)
            self.logger.info('using find-links of %s', findlinks)

        failed_downloads = 0
        for dist in self.dists:
            newloc = os.path.join(eggdir, os.path.basename(dist.location))
            if dist.platform is None:  # pure python
                if dist.location != newloc and dist.precedence != DEVELOP_DIST:
                    setuptools.archive_util.unpack_archive(dist.location, newloc)
            else:  # not pure python, so put in distrib-cache and build when user runs buildout
                fetched = index.download(dist.as_requirement(), self.bundle_cache)
                if fetched:
                    self.logger.debug('successfully downloaded %s' % fetched)
                else:
                    self.logger.error('failed to download distrib for %s' % dist.as_requirement())
                    failed_downloads += 1
        if failed_downloads:
            raise zc.buildout.UserError('%d failed downloads'
                                        % failed_downloads)
                
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
                    
        
        if self.readme:
            shutil.copy(self.readme, os.path.join(self.bundledir, 'README.txt'))

        if self.archive is True:
            tarname =  self._tarfile_name()
            self.logger.info('creating tar file %s' % tarname)
           
            tarf = tarfile.open(tarname, mode='w:gz')
            tarf.add(self.bundledir, arcname='%s-%s-py%s' %
                                    (self.options['bundle_name'], 
                                     self.bundle_version,
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

