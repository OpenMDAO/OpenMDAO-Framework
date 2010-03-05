
import os
import sys
import shutil
from distutils.util import get_platform
import tarfile
import zipfile
import logging
import fnmatch
import urllib2
import tempfile
from subprocess import check_call

import pkg_resources
from pkg_resources import Environment, WorkingSet, Distribution, Requirement, get_supported_platform
from setuptools.package_index import PackageIndex
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

if sys.platform == 'win32':
    def quote(c):
        if ' ' in c:
            return '"%s"' % c # work around spawn lamosity on windows
        else:
            return c
else:
    def quote (c):
        return c

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
        self.bundle_name = options['bundle_name']
        self.configs = options.get('buildout_configs') or ['buildout.cfg']
        fix = options.get('pin_versions').strip().lower()
        if fix == 'false':
            self.pin_versions = False
        else:
            self.pin_versions = True
        self.readme = options.get('readme','').strip()
        if not self.readme:
            self.logger.warning('No README file found!')
        
        self.excludeparts = [x for x in options['exclude_parts'].split() if x != '']
        self.parts = [x for x in buildout['buildout']['parts'].split() 
                               if x != '' and x not in self.excludeparts]

        archive = options.get('archive').strip().lower()
        if archive == 'false':
            self.archive = False
        else:
            self.archive = True
            
        self.bdists = set([x.strip() for x in options.get('bdists', '').split('\n') 
                                 if x.strip()])
        extra_stuff = [x.strip() for x in options.get('extra_data', '').split('\n') 
                          if x.strip()]
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
        
        if 'sphinxbuild' in bo:
            bo['sphinxbuild']['build'] = 'docscript'
        
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
                f.write('\n\n[%s]\n' % sect)
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

    def _pre_install(self, distloc):
        self.logger.info("pre-installing %s" % os.path.basename(distloc))
        if distloc.endswith('.egg'):
            self.logger.debug("unpacking archive %s to %s" %
                              (os.path.join(self.bundle_cache, distloc),
                               os.path.join(self.bundledir,'buildout','eggs',
                                            distloc)))
            setuptools.archive_util.unpack_archive(distloc,
                                                   os.path.join(self.bundledir,
                                                                'buildout','eggs',
                                                                os.path.basename(distloc)))
        else:
            cmd = "from setuptools.command.easy_install import main; main()"
            check_call([sys.executable, '-c', cmd, 
                        '-maqNxd',
                        os.path.join(self.bundledir,'buildout','eggs'), 
                        distloc], env=os.environ)
        # get rid of the distribution archive
        os.remove(os.path.join(self.bundle_cache, distloc))

    def _build_dist(self, build_type):
        cmd = '%s setup.py %s -d %s' % (self.buildout['buildout']['executable'],
                                              build_type, self.bundle_cache)
        p = Popen(cmd, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
        out = p.communicate()[0]
        ret = p.returncode
        if ret != 0:
            self.logger.error(out)
            raise zc.buildout.UserError(
                 'error while building egg in %s (return code=%d)'
                  % (os.getcwd(), ret))
    
    def _build_dev_eggs(self, startdir):
        """Build real eggs out of all of the develop eggs."""
        env = Environment()
        for degg in self.develop:
            os.chdir(startdir)
            absegg = os.path.abspath(degg)
            self.logger.debug('building egg in %s' % degg)
            os.chdir(degg)
            # clean up any old builds
            if os.path.exists('build'):
                shutil.rmtree('build')
            
            self._build_dist('bdist_egg')
            dist = Environment([self.bundle_cache])[os.path.basename(degg)][0]
            if dist.platform is None:  # pure python distrib. 'pre-install' it in eggs dir
                self._pre_install(os.path.join(self.bundle_cache, dist.location))
            else:
                if dist.project_name in self.bdists:
                    self.is_binary = True
                else:
                    os.remove(dist.location)
                    self._build_dist('sdist')

                        
    def _tarfile_name(self):
        """ Returns name of tar file to be created. """
        pyver = 'py%s' % sys.version[:3]
        if self.is_binary:
            pyver = pyver + '-%s' % get_supported_platform()
        return os.path.join(self.bundledir, '%s-bundle-%s-%s.tar.gz' % 
                                            (self.bundle_name,
                                             self.bundle_version,
                                             pyver))

    def install(self):
        self.is_binary = False
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
        try:
            spath = self.buildout['buildout']['extra-paths']
            search_path = [x.strip() for x in spath.split('\n') if x.strip()]
        except KeyError:
            search_path = []
        env = Environment([self.buildout['buildout']['eggs-directory'], 
                           eggdir,
                           os.path.join(self.buildout['buildout']['directory'],
                                        'setup'),
                           self.buildout['buildout']['develop-eggs-directory']]+search_path)
        reqs = [Requirement.parse(x.strip()) for x in self.options['eggs'].split()]
        self.dists = WorkingSet().resolve(reqs, env)
        
        self._setup_buildout_dir()  
        
        # Check that we can contact the egg server.
        url = self.buildout['buildout'].get('index',
                                            'http://pypi.python.org/simple')
        self.logger.debug('using URL %s', url)
        self._check_url(url)
        self.logger.debug('    search path %s', search_path)

        index = PackageIndex(url, search_path=[])
        try:
            flinks = self.buildout['buildout']['find-links']
        except KeyError:
            pass
        else:
            findlinks = [x.strip() for x in flinks.split('\n') if x.strip()]
            index.add_find_links(findlinks)
            self.logger.debug('    find-links %s', findlinks)
            for link in findlinks:
                self._check_url(link)

        failed_downloads = 0
        devs = set([os.path.basename(x) for x in self.develop])
        for dist in self.dists:
            rep = dist.project_name.replace('-','_')
            if rep in devs: # skip develop eggs we've already included
                continue
            self.logger.info('processing %s...', dist.as_requirement())
            newloc = os.path.join(eggdir, os.path.basename(dist.location))
            if dist.platform is None:  # pure python
                src = False
            else:
                src = True
                self.logger.debug('getting source distrib for project %s' % dist.project_name)
            fetched = getattr(index.fetch_distribution(dist.as_requirement(), self.bundle_cache, 
                                               source=src, develop_ok=True, force_scan=True), 
                              'location', None)
                
            if fetched:
                self.logger.debug('    downloaded %s', fetched)
                bpath = os.path.join(self.bundle_cache, os.path.basename(fetched))
                if not os.path.exists(bpath):
                    if os.path.isdir(fetched):
                        self.logger.debug("DIR FOUND: %s" % fetched)
                    else:
                        shutil.copy(fetched, bpath)
                if dist.platform is None:  # pure python
                    if not os.path.exists(os.path.join(self.bundledir, 
                                                       'buildout', 
                                                       'eggs',os.path.basename(fetched))):
                        self._pre_install(bpath)
                elif dist.project_name in self.bdists:
                    # it's a source dist that we have to make into a bdist_egg
                    self.sdist_to_bdist_egg(bpath, delete=True)
            else:
                self.logger.error('    download failed')
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
                                    (self.bundle_name, 
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

    def _check_url(self, url):
        """ Check that we can contact a URL. """
#        proxy_support = urllib2.ProxyHandler({})
#        opener = urllib2.build_opener(proxy_support)
#        try:
#            opener.open(url).read()
        try:
            urllib2.urlopen(url, timeout=10)
        except urllib2.URLError, exc:
            msg = "Can't contact egg server at '%s': %s" % (url, exc)
            raise zc.buildout.UserError(msg)

    def sdist_to_bdist_egg(self, sdist_path, delete=False):
        """Take an sdist and replace it with a bdist_egg"""
        self.is_binary = True
        self.logger.debug("Converting sdist %s to a bdist_egg" % sdist_path)
        tmpdir = tempfile.mkdtemp()
        cdir = os.getcwd()
        try:
            # unpack the sdist
            if sdist_path.endswith('.tar.gz') or sdist_path.endswith('.tar'):
                tarf = tarfile.open(sdist_path)
                tarf.extractall(path=tmpdir)
            elif sdist_path.endswith('.zip'):
                zfile = zipfile.ZipFile(sdist_path, 'r')
                zfile.extractall(tmpdir)
            else:
                raise RuntimeError("Don't know how to unpack file %s" % sdist_path)
            os.chdir(tmpdir)
            dirs = os.listdir(tmpdir)
            if len(dirs) == 1:
                os.chdir(dirs[0])
            setupnames = ['setupegg.py', 'setup_egg.py', 'setup.py']
            for name in setupnames:
                if os.path.isfile(name):
                    setup = name
                    break
            else:
                setup = None
            if not setup:
                raise RuntimeError("Can't find setup.py file in %s" % os.getcwd())
            try:
                check_call([sys.executable, 
                            setup, 'bdist_egg', '-d', '%s' % os.path.dirname(sdist_path)], 
                           env=os.environ)
            except:
                # could be an old school distutils setup.py file. Try tricking it into
                # using setuptools.setup so we can build an egg
                setups = [' setup ',' setup,', ',setup ', ',setup,']
                with open(setup, 'r') as fin:
                    with open('___setup.py', 'w') as fout:
                        found = False
                        for line in fin:
                            if not found:
                                if line.startswith('import ') or ' import ' in line:
                                    for s in setups:
                                        if s in line:
                                            found = True
                                            indent = line.find('from')
                                            if indent < 0:
                                                indent = line.find('import')
                                fout.write(line+'\n')
                                if found:
                                    fout.write(' '*indent)
                                    fout.write('from setuptools import setup\n')
                            else:
                                fout.write(line+'\n')
                
                check_call([sys.executable, 
                            '___setup.py', 'bdist_egg', '-d', '%s' % 
                                 os.path.dirname(sdist_path)], 
                           env=os.environ)
            if delete:
                os.remove(sdist_path)
        finally:
            shutil.rmtree(tmpdir)
            os.chdir(cdir)
    
    update = install

