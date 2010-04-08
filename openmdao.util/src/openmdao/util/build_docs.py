
import os
import os.path
import sys
import fnmatch
import logging
import StringIO
from subprocess import Popen, PIPE, STDOUT
from pkg_resources import Environment, WorkingSet, Requirement, working_set

from openmdao.util.dumpdistmeta import get_dist_metadata

# Put configuration info here
src_mods = []

#def _get_entrypt_script_str(req, entry_pt_name, *args):
    #return """
##!%(exe)s
## EASY-INSTALL-ENTRY-SCRIPT: '%(req)s','console_scripts','%(ep_name)s'
#__requires__ = '%(req)s'
#import sys
#from pkg_resources import load_entry_point

#sys.exit(
   #load_entry_point('%(req)s', 'console_scripts', '%(ep_name)s')(*%(args)s)
#)
    #""" % { 'req':req, 'ep_name':entry_pt_name, 'args':args }

def _find_repo_top():
    """Return the top of the current bazaar repo, or raise an
    exception if we're not in a repo
    """
    start = os.getcwd()
    location = start
    while location:
        if '.bzr' in os.listdir(location):
            return location
        location = os.path.dirname(location)
    raise RuntimeError('ERROR: %s is not inside of a bazaar repository' % start)

# set all of our global configuration parameters
branchdir = _find_repo_top()
docdir = os.path.join(branchdir, 'docs')
bindir = os.path.dirname(sys.executable)
srcmods = [
]
logger = logging.getLogger()
packages = [
    'openmdao.main',
    'openmdao.lib',
    'openmdao.util',
    'openmdao.units'
]

def _mod_sphinx_info(mod, outfile, show_undoc=False):
    """Write out enough info for Sphinx to autodocument
    a module.
    """
    name = os.path.splitext(mod.replace('/', '.'))[0]
    short = os.path.basename(name)
    modbase = short.split('.').pop()
    
    outfile.write('.. index:: %s.py\n\n' % modbase)
    outfile.write('.. _%s.py:\n\n' % short)
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

                
def _pkg_sphinx_info(startdir, pkg, outfile, show_undoc=False,
                    underline='-'):
    """Generate Sphinx autodoc directives for all of the modules in 
    the given package.
    
    """
    # locate the package directory
    topdir = pkg
    pkgdir = pkg
    
    dist = working_set.find(Requirement.parse(pkg))
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
        if os.path.basename(name) == 'releaseinfo.py':
            continue
        for ex in exdirs:
            if  name.startswith('%s/' % ex) or '/%s/'%ex in name:
                break
        else:       
            _mod_sphinx_info(name, outfile, show_undoc=show_undoc)


#def test_sphinx_docs(*args):
    #"""Run the builtin source code (and doctest) stuff in sphinx for the
    #OpenMDAO documentation.
    #"""
    

def _write_src_docs():
    # first, clean up the old stuff, if any
    pkgdir = os.path.join(docdir, 'srcdocs', 'packages')
    moddir = os.path.join(docdir, 'srcdocs', 'modules')
    
    for name in os.listdir(pkgdir):
        os.remove(os.path.join(pkgdir, name))
    
    for name in os.listdir(moddir):
        os.remove(os.path.join(moddir, name))
    
    for pack in packages:
        print 'creating autodoc file for %s' % pack
        with open(os.path.join(pkgdir, pack+'.rst'), 'w') as f:
            _pkg_sphinx_info(branchdir, pack, f, 
                             show_undoc=True, underline='-')
    
    for src in srcmods:
        with open(os.path.join(docdir, 'srcdocs', 'modules',
                              os.path.basename(src)+'.rst'), 'w') as f:
            logger.info('creating autodoc file for %s' % src)
            _mod_sphinx_info(os.path.basename(src), f)

def build_docs():
    """An entry point (build_docs) points to this.  It generates the Sphinx
    documentation for openmdao.
    """
    startdir = os.getcwd()
    if not os.path.isdir(docdir):
        raise RuntimeError('doc directory '+docdir+' not found')
    
    _write_src_docs()
    _make_license_table()
    
    os.chdir(docdir)
    try:
        # make necessary directories if they aren't already there
        if not os.path.isdir(os.path.join('_build', 'html')):
            os.makedirs(os.path.join('_build', 'html'))
        if not os.path.isdir(os.path.join('_build', 'doctrees')):
            os.makedirs(os.path.join('_build', 'doctrees'))
    
        import sphinx
        sphinx.main(argv=['-P', '-b', 'html', '-d', 
                          os.path.join(docdir, '_build', 'doctrees'), 
                          docdir, os.path.join(docdir, '_build', 'html')])
    finally:
        os.chdir(startdir)

def view_docs(browser=None):
    """An entry point (docs) points to this. It just pops up a browser to 
    view the openmdao sphinx docs. If the docs are not already built, it
    builds them first.
    """
    if not browser:
        for arg in sys.argv:
            if arg.startswith('--browser='):
                browser = arg.split('=')[-1].strip()
                
    import webbrowser
    wb = webbrowser.get(browser)
    idxpath = os.path.join(docdir, '_build', 'html', 'index.html')
    if not os.path.isfile(idxpath):
        build_docs()
    wb.open(idxpath)


#def test_docs():
    ## create the testdocs script
    #tstscript = zc.buildout.easy_install.scripts(
        #['Sphinx'], working_set, 
        #sys.executable, os.path.dirname(bspath), { 'sphinx-build': 'testdocs' },
        #arguments= "argv=['-P', '-b', 'doctest', '-d', r'%s', r'%s', r'%s']" %
                    #(os.path.abspath(os.path.join('_build', "doctrees")),
                     #os.path.abspath(docdir), 
                     #os.path.abspath(os.path.join('_build', "html"))))
        
    ## create a unit test for the source code found in the docs
    #utdir = os.path.join(self.buildout['buildout']['directory'],
                         #'parts', self.name)
    #if not os.path.exists(utdir):
        #os.makedirs(utdir)
    #utname = os.path.join(utdir,'test_docs.py')
    #utest = open(utname, 'w')
    #utest.write("""
#import unittest
#import os
#from os.path import join
#from subprocess import Popen, PIPE, STDOUT

#class SphinxDocsTestCase(unittest.TestCase):
    #def test_docs(self):
        #p = Popen(r'%s', stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
        #output = p.communicate()[0]
        #retval = p.returncode
        #if not output.strip().endswith('build succeeded.'):
            #self.fail('problem in documentation source code examples:\\n'+output)
            #""" % os.path.join(bindir, 'testdocs')
    #)

    
def _get_border_line(numcols, colwidths, char):
    parts = []
    for i in range(numcols):
        parts.append(char*colwidths[i])
        parts.append(' ')
    parts.append('\n')
    return ''.join(parts)

def _get_table_cell( data, colwidth):
    return data+' '*(colwidth-len(data))
    
def _make_license_table(reqs=None):
    """
    Generates a file in docs/licenses/licenses_table.rst that
    contains a restructured text table with the name, license, and home-page of
    all distributions that openmdao depends on.
    """
    meta_names = ['name','license','home-page']
    headers = ['**Distribs Used by OpenMDAO**',
               '**License**',
               '**Link**']
    numcols = len(meta_names)
    data_templates = ["%s", "%s", "%s"]
    col_spacer = ' '
    max_col_width = 80
    excludes = [] #["openmdao.*"]
    license_fname = os.path.join(docdir,'licenses','licenses_table.txt')
    
    if reqs is None:
        reqs = [Requirement.parse(p) for p in packages]
    dists = working_set.resolve(reqs)
        
    metadict = {}
    for dist in dists:
        metadict[dist.project_name] = get_dist_metadata(dist)
    to_remove = set()
    for pattern in excludes:
        to_remove.update(fnmatch.filter(metadict.keys(), pattern))
    for rem in to_remove:
        del metadict[rem]
    for meta in metadict.values():
        for i,name in enumerate(meta_names):
            meta[name] = data_templates[i] % str(meta[name])
    # figure out sizes of table columns
    colwidths = [len(s)+1 for s in headers]
    for i,name in enumerate(meta_names):
        sz = max([len(m[name]) for m in metadict.values()])+1
        sz = min(sz, max_col_width)
        colwidths[i] = max(colwidths[i], sz)
    
    with open(license_fname, 'wb') as outfile:
        # write header
        outfile.write(_get_border_line(numcols, colwidths, char='='))
        for i,header in enumerate(headers):
            outfile.write(header+' '*(colwidths[i]-len(header)))
            outfile.write(col_spacer)
        outfile.write('\n')
        outfile.write(_get_border_line(numcols, colwidths, char='='))
        
        # write table data
        tups = [(k,v) for k,v in metadict.items()]
        tups = sorted(tups, lambda x,y: cmp(x[0].lower(), y[0].lower()))
        for j,tup in enumerate(tups):
            for i,name in enumerate(meta_names):
                outfile.write(_get_table_cell(tup[1][name], colwidths[i]))
                outfile.write(col_spacer)
            outfile.write('\n')
            if j<len(tups)-1:
                outfile.write(_get_border_line(numcols, colwidths, char='-'))
            
        # bottom border
        outfile.write(_get_border_line(numcols, colwidths, char='='))
        outfile.write('\n')
