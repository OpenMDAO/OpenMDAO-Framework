
import os
import os.path
import sys
import fnmatch
import logging
import rfc822
import StringIO

from pkg_resources import Environment
from pkg_resources import get_distribution, resource_listdir
from pkg_resources import WorkingSet, Requirement

from openmdao.util.procutil import run_command

script_template = """\
#!%(python)s

import webbrowser

wb = webbrowser.get("%(browser)s")
wb.open(r"%(index)s")

"""



def mod_sphinx_info(mod, outfile, show_undoc=False):
    name = os.path.splitext(mod.replace('/', '.'))[0]
    short = os.path.basename(name)
    
    print >> outfile, '%s.py' % short.split('.').pop()
    print >> outfile, '_'*(3+len(short.split('.').pop()))+'\n'
    print >> outfile, '.. automodule:: %s' % short
    print >> outfile, '   :members:'
    if show_undoc:
        print >> outfile, '   :undoc-members:'
    print >> outfile, '   :show-inheritance:\n\n'


def _match(name, inlist):
    """Return True if the given name matches any of the
    contents of the list of glob patterns inlist.
    """
    for pat in inlist:
        if fnmatch.fnmatchcase(name, pat):
            return True
    return False
    
    
def get_resource_files(dist, exList=None, incList=None, dirname=''):
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
            for r in get_resource_files(dist, exlist, inclist, respath):
                if _match(r, inclist) and not _match(r, exlist):
                    yield r
        else:
            if _match(respath, inclist) and not _match(respath, exlist):
                yield respath

                
def get_metadata(dist, dirname=''):
    """Retrieve metadata from within a distribution and return it as
    a generator of tuples of the form (metadata_key, value).
    """
    
    for name in dist.metadata_listdir(dirname):
        if dirname != '':
            path = '/'.join([dirname, name])
        else:
            path = name
        if dist.metadata_isdir(path):
            for md in get_metadata(dist, path):
                yield md
        elif name.endswith('.txt'):
            yield (path[:-4], [x.strip() for x in dist.get_metadata(path).splitlines() if x.strip() != ''])
        elif name == 'PKG-INFO':
            instr = StringIO.StringIO(dist.get_metadata(name))
            message = rfc822.Message(instr)
            for k,v in message.items():
                yield (k, v)
        elif name == 'not-zip-safe':
            yield ('zip-safe', False)
        elif name == 'zip-safe':
            yield ('zip-safe', True)
        else:
            yield (path, dist.get_metadata(path))
                
            
def pkg_sphinx_info(env,startdir, pkg, outfile, show_undoc=False, underline='-'):
    """Generate Sphinx autodoc directives for all of the modules in 
    the given package.
    
    """
    # locate the package directory
    topdir = pkg
    pkgdir = pkg
    
    ws = WorkingSet()
    dist = env.best_match(Requirement.parse(pkg), ws)

    print >> outfile, 'Package %s' % pkg
    print >> outfile, underline*(len('Package ')+len(pkg))
    print >> outfile, '\n\n'
    
    names = list(get_resource_files(dist,['*__init__.py','*setup.py','*test_*.py'],
                                    ['*.py']))            
    names.sort()
    
    exdirs = ['build','examples']
            
    for name in names:
        for ex in exdirs:
            if  name.startswith('%s/' % ex) or '/%s/'%ex in name:
                break
        else:       
            mod_sphinx_info(name, outfile, show_undoc=show_undoc)

# TODO: add metadata info to doc page        
#    for md,val in get_metadata(dist):
#        print "%s:\n%s\n"%(md,val)


if __name__ == '__main__':
    from optparse import OptionParser
    
    parser = OptionParser()
    parser.add_option("-u","", action="store_true", dest="show_undoc",
                      help="show undocumented members")
    parser.add_option("-o","", action="store", type='string', dest="out",
                      help="output filename (defaults to stdout)")
    (options, args) = parser.parse_args(sys.argv[1:])
    
    if options.out:
        outf = open(options.out, 'w')
    else:
        outf = sys.stdout
    
    if len(args) == 1:
        pkg_sphinx_info(args[0], outf, options.show_undoc)
    else:
        parser.print_help()
        sys.exit(-1)


    
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
        self.interpreter = os.path.join(buildout['buildout']['bin-directory'], 'python')
        self.executable = buildout['buildout']['executable']
        
        self.packages = options.get('packages') or ''  
        self.docdir = options.get('doc_dir') or 'docs'
        self.builddir = options.get('build_dir') or '_build' 
        self.builder = options.get('build_script') or os.path.join(
                                                           self.branchdir,
                                                           'docs',
                                                           'python-scripts',
                                                           'sphinx-build')
        self.egg_dir = buildout['buildout']['eggs-directory']
        self.dev_egg_dir = buildout['buildout']['develop-eggs-directory']


    def _write_src_docs(self):
        for pack in self.packages.split():
            self.logger.info('creating autodoc file for %s' % pack)
            f = open(os.path.join(self.docdir,'srcdocs','packages',
                                  pack+'.rst'),'w')
            pkg_sphinx_info(self.env,self.branchdir, pack, f, 
                            show_undoc=True, underline='-')
            f.close()

               
    def install(self):
        dev_eggs = fnmatch.filter(os.listdir(self.dev_egg_dir),'*.egg-link')
        # grab the first line of each dev egg link file
        self.dev_eggs = [
           open(os.path.join(self.dev_egg_dir,f),'r').readlines()[0].strip() 
                                                               for f in dev_eggs]
        self.env = Environment(self.dev_eggs+[os.path.join(self.egg_dir,x) 
                          for x in fnmatch.filter(os.listdir(self.egg_dir),'*.egg')])
        
        startdir = os.getcwd()
        if not os.path.isdir(self.docdir):
            self.docdir = os.path.join(self.branchdir, self.docdir)
        if not os.path.isdir(self.docdir):
            raise RuntimeError('doc directory '+self.docdir+' not found')
            
        self._write_src_docs()
            
        os.chdir(self.docdir)        
        
        # make necessary directories if they aren't already there
        if not os.path.isdir(os.path.join(self.builddir,'html')):
            os.makedirs(os.path.join(self.builddir,'html'))
        if not os.path.isdir(os.path.join(self.builddir,'doctrees')):
            os.makedirs(os.path.join(self.builddir,'doctrees'))
            
        # build the docs using Sphinx
        try:
            sys.path[0:0] = [os.path.abspath('python-scripts')]
            out,ret = run_command('%s %s -P -b html -d %s . %s' %
                                    (self.interpreter, self.builder, 
                                     os.path.join(self.builddir,'doctrees'), 
                                     os.path.join(self.builddir,'html')))
        except Exception, err:
            self.logger.error(out)
            self.logger.error(str(err))
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
            browser = self.options.get('browser') or 'windows-default'
        else:
            scriptname = os.path.join(self.buildout['buildout']['directory'],
                                     'bin','docs')
            browser = self.options.get('browser') or 'firefox'
        script = open(scriptname, 'w')
        
        idxpath = os.path.join(self.branchdir, self.docdir, self.builddir,
                               'html','index.html')
        script.write(script_template % dict(python=self.executable,
                                            browser=browser,
                                            index=idxpath))
        script.close()
        os.chmod(scriptname, 0775)
        
        return [scriptname]
    
    
    update = install  

