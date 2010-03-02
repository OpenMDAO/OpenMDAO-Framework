
import os
import os.path
import sys
import fnmatch
import logging
import rfc822
import StringIO
import zc.buildout
from subprocess import Popen, PIPE, STDOUT
from pkg_resources import Environment, WorkingSet, Requirement, working_set


def _mod_sphinx_info(mod, outfile, show_undoc=False):
    """Write out enough info for Sphinx to autodocument
    a module.
    """
    name = os.path.splitext(mod.replace('/', '.'))[0]
    short = os.path.basename(name)
    modbase = short.split('.').pop()
    
    outfile.write('.. index:: %s.py\n\n'%modbase)
    outfile.write('.. _%s.py:\n\n'%modbase)
    outfile.write('%s.py\n' % modbase)
    outfile.write('_'*(3+len(short.split('.').pop()))+'\n\n')
    outfile.write('.. automodule:: %s\n' % short)
    outfile.write('   :members:\n')
    if show_undoc:
        outfile.write('   :undoc-members:\n')
    outfile.write('   :show-inheritance:\n\n\n')


def _match(name, inlist):
    """Return True if the given name matches any of the
    contents of the list of glob patterns inlist.
    """
    for pat in inlist:
        if fnmatch.fnmatchcase(name, pat):
            return True
    return False
    
    
def _get_resource_files(dist, exList=None, incList=None, dirname=''):
    """Retrieve resource file pathnames from within a distribution."""
    
    exlist = exList or []
    inclist = incList or ['*']
    
    flist = dist.resource_listdir(dirname)
        
    for res in flist:
        if dirname != '':
            respath = '/'.join([dirname, res])
        else:
            respath = res
        if dist.resource_isdir(respath):
            for r in _get_resource_files(dist, exlist, inclist, respath):
                if _match(r, inclist) and not _match(r, exlist):
                    yield r
        else:
            if _match(respath, inclist) and not _match(respath, exlist):
                yield respath

                
def _get_metadata(dist, dirname=''):
    """Retrieve metadata from within a distribution and return it as
    a generator of tuples of the form (metadata_key, value).
    """
    
    for name in dist.metadata_listdir(dirname):
        if dirname != '':
            path = '/'.join([dirname, name])
        else:
            path = name
        if dist.metadata_isdir(path):
            for md in _get_metadata(dist, path):
                yield md
        elif name.endswith('.txt'):
            yield (path[:-4], [x.strip() for x in dist.get_metadata(path).splitlines() if x.strip() != ''])
        elif name == 'PKG-INFO':
            instr = StringIO.StringIO(dist.get_metadata(name))
            message = rfc822.Message(instr)
            for k, v in message.items():
                yield (k, v)
        elif name == 'not-zip-safe':
            yield ('zip-safe', False)
        elif name == 'zip-safe':
            yield ('zip-safe', True)
        else:
            yield (path, dist.get_metadata(path))
                
            
def _pkg_sphinx_info(ws, startdir, pkg, outfile, show_undoc=False,
                    underline='-'):
    """Generate Sphinx autodoc directives for all of the modules in 
    the given package.
    
    """
    # locate the package directory
    topdir = pkg
    pkgdir = pkg
    
    #ws = WorkingSet()
    #dist = env.best_match(Requirement.parse(pkg), ws)
    dist = ws.find(Requirement.parse(pkg))
    if dist is None:
        logging.error('no dist found for Requirement(%s)'%pkg)
    print >> outfile, 'Package %s' % pkg
    print >> outfile, underline*(len('Package ')+len(pkg))
    print >> outfile, '\n\n'
    
    names = list(_get_resource_files(dist,
                                    ['*__init__.py','*setup.py','*/test/*.py'],
                                    ['*.py']))            
    names.sort()
    
    exdirs = ['build', 'examples']
            
    for name in names:
        for ex in exdirs:
            if  name.startswith('%s/' % ex) or '/%s/'%ex in name:
                break
        else:       
            _mod_sphinx_info(name, outfile, show_undoc=show_undoc)

# TODO: add metadata info to doc page        
#    for md,val in _get_metadata(dist):
#        print "%s:\n%s\n"%(md,val)


def test_sphinx_docs(*args):
    """Run the builtin source code (and doctest) stuff in sphinx for the
    OpenMDAO documentation.
    """
    
def gen_docs_script(buildout, bindir, idxpath, browser=''):
    """Create a bin/docs script that displays docs in a browser"""
    if sys.platform == 'win32':
        scriptname = os.path.join(bindir, 'docs.py')
        bat = open(os.path.join(bindir, 'docs.bat'), 'w')
        bat.write("@echo off\npython %s" % scriptname)
        bat.close()
    else:
        scriptname = os.path.join(bindir, 'docs')
                                 
    if browser != '':
        browser = "'%s'" % browser
        
    script = open(scriptname, 'w')
    script.write("""\
#!%(python)s

import webbrowser

wb = webbrowser.get(%(browser)s)
wb.open(r"%(index)s")
    """ % dict(python=buildout['buildout']['executable'],
               browser=browser,
               index=idxpath))
    script.close()
    try:
        os.chmod(scriptname, 0775)
    except (AttributeError, os.error):
        pass
    return scriptname

class SphinxBuild(object):
    """Build Sphinx documentation and create a script to bring up the
    documentation in a browser. This is specific to the OpenMDAO Sphinx docs and
    is not a  general purpose Sphinx building recipe.
    
    The following options are supported:

    **eggs**
        list of dependencies that will be added to sys.path so autodoc can
        find all referenced modules
        
    *packages*
        names of packages to generate autodocumentation for
        
    *srcdirs*
        directories to scan for python files to be autodocumented
        
    *srcmods*
        modules to be autodocumented
        
    *doc_dir*
        top level directory where sphinx documentation is found
        
    *build_dir*
        directory where sphinx build resides
        
    *build*
        If set to false, won't rebuild the documentation and the user
        will have to run the sphinx-build script manually to update
        the docs. Defaults to true.
        
    """

    def __init__(self, buildout, name, options):
        self.name = name
        self.buildout = buildout
        self.options = options
        self.logger = logging.getLogger(name)
        self.branchdir = os.path.split(buildout['buildout']['directory'])[0]
        self.interpreter = os.path.join(buildout['buildout']['bin-directory'],
                                        'python')
        self.executable = buildout['buildout']['executable']
        
        self.packages = options.get('packages', '')
        self.srcdirs = options.get('srcdirs', '')
        self.srcmods = options.get('srcmods', '')
        self.docdir = options.get('doc_dir', 'docs')
        self.builddir = options.get('build_dir', '_build')
        self.build = set([x.strip().lower() for x in options['build'].split('\n') 
                                     if x.strip()])
        dev_egg_dir = buildout['buildout']['develop-eggs-directory']
        dev_eggs = fnmatch.filter(os.listdir(dev_egg_dir),'*.egg-link')
        # grab the first line of each dev egg link file
        self.dev_eggs = [open(os.path.join(dev_egg_dir,f),'r').readlines()[0].strip() 
                            for f in dev_eggs]
        self.working_set = None
                            


    def _write_src_docs(self):
        # first, clean up the old stuff, if any
        pkgdir = os.path.join(self.docdir, 'srcdocs', 'packages')
        moddir = os.path.join(self.docdir, 'srcdocs', 'modules')
        
        for name in os.listdir(pkgdir):
            os.remove(os.path.join(pkgdir, name))
        
        for name in os.listdir(moddir):
            os.remove(os.path.join(moddir, name))
        
        for pack in self.packages.split():
            self.logger.info('creating autodoc file for %s' % pack)
            f = open(os.path.join(pkgdir, pack+'.rst'), 'w')
            _pkg_sphinx_info(self.working_set, self.branchdir, pack, f, 
                            show_undoc=True, underline='-')
            f.close()
        
        srcs = []
        for srcdir in self.srcdirs.split():
            for src in os.listdir(os.path.join(self.branchdir, srcdir)):
                if src.endswith('.py'):
                    srcs.append(os.path.join(srcdir, src))
                    
        srcs.extend(self.srcmods.split())
        
        for src in srcs:
            f = open(os.path.join(self.docdir, 'srcdocs', 'modules',
                                  os.path.basename(src)+'.rst'), 'w')
            self.logger.info('creating autodoc file for %s' % src)
            _mod_sphinx_info(os.path.basename(src), f)
            f.close()

    def install(self):
        scriptlist = []
        # build up a list of all egg dependencies resulting from our 'eggs' parameter
        env = Environment(self.dev_eggs+[self.buildout['buildout']['eggs-directory'],
                                         os.path.join(self.buildout['buildout']['directory'],
                                                      'setup')])
        reqs = [Requirement.parse(x.strip()) for x in self.options['eggs'].split()]
        self.depdists = WorkingSet().resolve(reqs, env)
        self.working_set = WorkingSet([d.location for d in self.depdists])
        bindir = os.path.join(self.buildout['buildout']['directory'], 'bin')
    
        startdir = os.getcwd()
        if not os.path.isdir(self.docdir):
            self.docdir = os.path.join(self.branchdir, self.docdir)
        if not os.path.isdir(self.docdir):
            raise RuntimeError('doc directory '+self.docdir+' not found')
        
        if 'docs' in self.build:
            self._write_src_docs()
            os.chdir(self.docdir)
            
            # make necessary directories if they aren't already there
            if not os.path.isdir(os.path.join(self.builddir, 'html')):
                os.makedirs(os.path.join(self.builddir, 'html'))
            if not os.path.isdir(os.path.join(self.builddir, 'doctrees')):
                os.makedirs(os.path.join(self.builddir, 'doctrees'))
            
            # create the sphinx-build script
            bspath = os.path.join(bindir, 'sphinx-build')
            
            bldscript = zc.buildout.easy_install.scripts(
                ['Sphinx'], self.working_set, 
                sys.executable, os.path.dirname(bspath), { 'sphinx-build': 'sphinx-build' },
                arguments= "argv=['-P', '-b', 'html', '-d', r'%s', r'%s', r'%s']" %
                            (os.path.abspath(os.path.join(self.builddir, "doctrees")),
                             os.path.abspath(self.docdir), 
                             os.path.abspath(os.path.join(self.builddir, "html"))))
            scriptlist.extend(bldscript)

        if 'test' in self.build:
            # create the testdocs script
            tstscript = zc.buildout.easy_install.scripts(
                ['Sphinx'], self.working_set, 
                sys.executable, os.path.dirname(bspath), { 'sphinx-build': 'testdocs' },
                arguments= "argv=['-P', '-b', 'doctest', '-d', r'%s', r'%s', r'%s']" %
                            (os.path.abspath(os.path.join(self.builddir, "doctrees")),
                             os.path.abspath(self.docdir), 
                             os.path.abspath(os.path.join(self.builddir, "html"))))
            scriptlist.extend(tstscript)

        if 'docs' in self.build:
            # build the docs using Sphinx
            try:
                p = Popen(bspath, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
                out = p.communicate()[0]
                ret = p.returncode
            except Exception, err:
                self.logger.error(str(err))
                raise zc.buildout.UserError('sphinx build failed')
            else:
                if ret == 0:
                    map(self.logger.info, out.splitlines())
                else:
                    map(self.logger.error, out.splitlines())
                    raise zc.buildout.UserError('sphinx build failed')
            finally:
                os.chdir(startdir)
        
        if 'docs' in self.build or 'docscript' in self.build:
            idxpath = os.path.join(self.branchdir, self.docdir, self.builddir,
                                   'html','index.html')
            scriptname = gen_docs_script(self.buildout, bindir, idxpath, 
                                         self.options.get('browser', ''))
            scriptlist.append(scriptname)
        
        if 'test' in self.build:
            # create a unit test for the source code found in the docs
            utdir = os.path.join(self.buildout['buildout']['directory'],
                                 'parts', self.name)
            if not os.path.exists(utdir):
                os.makedirs(utdir)
            utname = os.path.join(utdir,'test_docs.py')
            utest = open(utname, 'w')
            utest.write("""
import unittest
import os
from os.path import join
from subprocess import Popen, PIPE, STDOUT

class SphinxDocsTestCase(unittest.TestCase):
    def test_docs(self):
        p = Popen(r'%s', stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
        output = p.communicate()[0]
        retval = p.returncode
        if not output.strip().endswith('build succeeded.'):
            self.fail('problem in documentation source code examples:\\n'+output)
            """ % os.path.join(bindir, 'testdocs')
            )
            scriptlist.append(utname)
        
        return scriptlist

    update = install
        


