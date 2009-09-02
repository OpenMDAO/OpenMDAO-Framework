
import os
import os.path
import sys
import fnmatch
import logging
import rfc822
import StringIO
import zc.buildout

from pkg_resources import Environment, WorkingSet, Requirement, working_set

from openmdao.util.procutil import run_command


def _mod_sphinx_info(mod, outfile, show_undoc=False):
    """Write out enough info for Sphinx to autodocument
    a module.
    """
    name = os.path.splitext(mod.replace('/', '.'))[0]
    short = os.path.basename(name)
    modbase = short.split('.').pop()
    
    outfile.write('.. index:: _%s.py:\n\n'%modbase)
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


    
class SphinxBuild(object):
    """Build Sphinx documentation and create a script to bring up the
    documentation in a browser. This is specific to the OpenMDAO Sphinx docs and
    is not a  general purpose Sphinx building recipe.
    """

    def __init__(self, buildout, name, options):
        self.buildout = buildout
        self.name = name
        self.options = options
        self.logger = logging.getLogger(name)
        self.branchdir = os.path.split(buildout['buildout']['directory'])[0]
        self.interpreter = os.path.join(buildout['buildout']['bin-directory'],
                                        'python')
        self.executable = buildout['buildout']['executable']
        
        self.packages = options.get('packages') or ''  
        self.srcdirs = options.get('srcdirs') or ''  
        self.srcmods = options.get('srcmods') or ''  
        self.docdir = options.get('doc_dir') or 'docs'
        self.builddir = options.get('build_dir') or '_build' 
        self.builder = options.get('build_script') or os.path.join(
                                                           self.branchdir,
                                                           'docs',
                                                           'python-scripts',
                                                           'sphinx-build')
        self.egg_dir = buildout['buildout']['eggs-directory']
        self.dev_egg_dir = buildout['buildout']['develop-eggs-directory']
        self.working_set = None
        self.specs = [r.strip() for r in options.get('eggs', '').split('\n')
                         if r.strip()]


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
        #dev_eggs = fnmatch.filter(os.listdir(self.dev_egg_dir), '*.egg-link')
        ## grab the first line of each dev egg link file
        #self.dev_eggs = [
           #open(os.path.join(self.dev_egg_dir, f), 'r').readlines()[0].strip() 
           #for f in dev_eggs]
        #eggs = self.dev_eggs + \
               #[os.path.join(self.egg_dir, x) 
                #for x in fnmatch.filter(os.listdir(self.egg_dir), '*.egg')]
        #self.env = Environment(eggs)
        
        startdir = os.getcwd()
        if not os.path.isdir(self.docdir):
            self.docdir = os.path.join(self.branchdir, self.docdir)
        if not os.path.isdir(self.docdir):
            raise RuntimeError('doc directory '+self.docdir+' not found')
            
        self.working_set = zc.buildout.easy_install.install(self.specs, self.egg_dir,
                                                            executable=self.executable,
                                                            path=[self.egg_dir, self.dev_egg_dir],
                                                            newest=False)
        self._write_src_docs()
            
        os.chdir(self.docdir)        
        
        # make necessary directories if they aren't already there
        if not os.path.isdir(os.path.join(self.builddir, 'html')):
            os.makedirs(os.path.join(self.builddir, 'html'))
        if not os.path.isdir(os.path.join(self.builddir, 'doctrees')):
            os.makedirs(os.path.join(self.builddir, 'doctrees'))
       
        
        # create the builddocs script
        bspath = os.path.join(self.buildout['buildout']['directory'], 'bin',
                              'sphinx-build')
         
        scripts = zc.buildout.easy_install.scripts(
            ['Sphinx'], self.working_set, 
            sys.executable, os.path.dirname(bspath), { 'sphinx-build': 'sphinx-build' },
            arguments= "argv=['-P', '-b', 'html', '-d', r'%s', r'%s', r'%s']" %
                        (os.path.join(self.builddir, "doctrees"),
                         self.docdir, os.path.join(self.builddir, "html")))        

        # build the docs using Sphinx
        try:
            # run our little build script using the python interpreter that
            # knows how to find everything in the buildout, so it will use
            # the version of Sphinx in the buildout
            out, ret = run_command('%s %s' % (self.executable, bspath))
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
        
        # create a bin/docs script
        if sys.platform == 'win32':
            scriptname = os.path.join(self.buildout['buildout']['directory'],
                                     'bin','docs.py')
            bat = open(os.path.join(self.buildout['buildout']['directory'],
                                    'bin','docs.bat'), 'w')
            bat.write("@echo off\npython %s"%(scriptname,))
            bat.close()
            #browser = self.options.get('browser') or 'windows-default'
        else:
            scriptname = os.path.join(self.buildout['buildout']['directory'],
                                     'bin','docs')
                                     
        browser = self.options.get('browser') or ''
        if browser != '':
            browser = "'%s'" % browser
            
        script = open(scriptname, 'w')
        
        idxpath = os.path.join(self.branchdir, self.docdir, self.builddir,
                               'html','index.html')
        script.write("""\
#!%(python)s

import webbrowser

wb = webbrowser.get(%(browser)s)
wb.open(r"%(index)s")
        """ % dict(python=self.executable,
                   browser=browser,
                   index=idxpath))
        script.close()
        try:
            os.chmod(scriptname, 0775)
        except (AttributeError, os.error):
            pass
        
        return [scriptname]
    
    
    update = install  

    
if __name__ == '__main__':
    from optparse import OptionParser
    
    parser = OptionParser()
    parser.add_option("-u", "", action="store_true", dest="show_undoc",
                      help="show undocumented members")
    parser.add_option("-o", "", action="store", type='string', dest="out",
                      help="output filename (defaults to stdout)")
    (options, args) = parser.parse_args(sys.argv[1:])
    
    if options.out:
        outf = open(options.out, 'w')
    else:
        outf = sys.stdout
    
    if len(args) == 1:
        _pkg_sphinx_info(args[0], outf, options.show_undoc)
    else:
        parser.print_help()
        sys.exit(-1)


