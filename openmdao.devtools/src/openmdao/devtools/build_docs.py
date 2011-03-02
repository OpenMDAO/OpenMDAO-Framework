
import os
import os.path
import sys
import shutil
import fnmatch
import logging
import StringIO
import re
from subprocess import Popen, PIPE, STDOUT
from pkg_resources import Environment, WorkingSet, Requirement, working_set
import tarfile

from fabric.api import run, env, local, put, cd, get, settings
from fabric.state import connections

import sphinx

from openmdao.util.dumpdistmeta import get_dist_metadata
import openmdao.util.releaseinfo

# Specify modules and packages to be included in the OpenMDAO documentation here
srcmods = [
]

packages = [
    'openmdao.main',
    'openmdao.lib',
    'openmdao.util',
    'openmdao.units',
]


logger = logging.getLogger()

def get_revision():
    try:
        p = Popen('bzr log --short -r-1', 
                  stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
        out = p.communicate()[0]
        ret = p.returncode
    except:
        return '<unknown_rev>'
    else:
        return out.split()[0]

# set all of our global configuration parameters
def _get_dirnames():
    bindir = os.path.dirname(sys.executable)
    branchdir = os.path.dirname(os.path.dirname(bindir))
    docdir = os.path.join(branchdir, 'docs')
    return (branchdir, docdir, bindir)

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
    outfile.write('+'*(3+len(short.split('.').pop()))+'\n\n')
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
    
    # this behaves strangely, maybe because we use namespace pkgs?
    # mod points to module 'openmdao', not 'openmdao.whatever', so we
    # have to access 'whatever' through the 'openmdao' module
    mod = __import__(pkg)
    docs = getattr(mod, pkg.split('.')[1]).__doc__
    if docs:
        print >> outfile, docs, '\n'
    
    #excluding traits now since they need to be sorted separately
    names = list(_get_resource_files(dist,
                                    ['*__init__.py','*setup.py','*/test/*.py', '*datatypes*.py'],
                                    ['*.py']))
    names.sort()
    
    #wanted to sort traits separately based only on filenames despite differing paths
    traitz = list(_get_resource_files(dist, ['*__init__.py','*setup.py','*/test/*.py'], ['*datatypes*.py']))
    sorted_traitz = sorted(traitz, cmp=_compare_traits_path)
    
    names.extend(sorted_traitz)

    exdirs = ['build', 'examples']
    
    oldheader = None
    newheader = None

    for name in names:
        if os.path.basename(name) == 'releaseinfo.py':
            continue

        for ex in exdirs:
            if  name.startswith('%s/' % ex) or '/%s/'%ex in name:
                break
            else:       
                x = name.split('/')
                #kind of dirty, but the other sections doesn't need api header.
                if os.path.basename(name) == 'api.py' and x[1]=='lib':
                    newheader = 'api'
                if len(x) >= 4:
                    newheader =  x[2]
            if (oldheader != newheader):
                print >> outfile, '**%s**' % newheader.upper()
                print >> outfile, '_'*(4+len(newheader)) + '\n'
                oldheader = newheader
               
        _mod_sphinx_info(name, outfile, show_undoc=show_undoc)


def _write_src_docs(branchdir, docdir):
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

def build_docs(argv=None):
    """A script (openmdao_build_docs) points to this.  It generates the Sphinx
    documentation for openmdao.
    """
    if argv is None:
        argv = sys.argv[1:]
    if '-v' in argv:
        idx = argv.index('-v')
        version = argv[idx+1]
        shtitle = 'OpenMDAO Documentation v%s' % version
    else:
        #version = openmdao.util.releaseinfo.__version__
        version = 'rev %s' % get_revision()
        shtitle = 'OpenMDAO Documentation (%s)' % version
    
    branchdir, docdir, bindir =_get_dirnames()

    startdir = os.getcwd()
    if not os.path.isdir(docdir):
        raise RuntimeError('doc directory '+docdir+' not found')
    
    _write_src_docs(branchdir, docdir)
    _make_license_table(docdir)
    
    os.chdir(docdir)
    try:
        # make necessary directories 
        if os.path.isdir(os.path.join('_build', 'html')):
            shutil.rmtree(os.path.join('_build','html'))
        if os.path.isdir(os.path.join('_build', 'doctrees')):
            shutil.rmtree(os.path.join('_build', 'doctrees'))
        os.makedirs(os.path.join('_build', 'html'))
        os.makedirs(os.path.join('_build', 'doctrees'))
                    
        sphinx.main(argv=['-P', '-b', 'html',
                          '-Dhtml_short_title=%s' % shtitle,
                          '-Dversion=%s' % version,
                          '-Drelease=%s' % version,
                          '-d', os.path.join(docdir, '_build', 'doctrees'), 
                          docdir, os.path.join(docdir, '_build', 'html')])
    finally:
        os.chdir(startdir)

def view_docs(browser=None):
    """A script (openmdao_docs) points to this. It just pops up a browser to 
    view the openmdao sphinx docs. If the docs are not already built, it
    builds them before viewing, but if the docs already exist, it's not smart enough
    to rebuild them if they've changed since the last build.
    """
    if not browser:
        for arg in sys.argv:
            if arg.startswith('--browser='):
                browser = arg.split('=')[-1].strip()
                
    branchdir, docdir, bindir =_get_dirnames()
    idxpath = os.path.join(docdir, '_build', 'html', 'index.html')
    if not os.path.isfile(idxpath):
        build_docs()
    
    import webbrowser
    wb = webbrowser.get(browser)
    wb.open(idxpath)

def push_docs():
    """A script (push_docs) points to this. It pushes the current copy of the docs up
    to the development doc area on openmdao.org.
    """
    startdir = os.getcwd()
    branchdir, docdir, bindir =_get_dirnames()
    idxpath = os.path.join(docdir, '_build', 'html', 'index.html')
    if not os.path.isfile(idxpath):
        build_docs()

    try:
        os.chdir(os.path.join(docdir, '_build'))
        try:
            if os.path.exists('docs.tar.gz'):
                os.remove('docs.tar.gz')
            archive = tarfile.open('docs.tar.gz', 'w:gz')
            archive.add('html')
            archive.close()
        finally:
            os.chdir(startdir)
        
        with settings(host_string='openmdao@web103.webfaction.com'):
            # tar up the docs so we can upload them to the server
            # put the docs on the server and untar them
            put(os.path.join(docdir,'_build','docs.tar.gz'), 'downloads/docs.tar.gz')
            with cd('downloads'):
                run('tar xzf docs.tar.gz')
                run('rm -rf dev_docs')
                run('mv html dev_docs')
                run('rm -f docs.tar.gz')
    finally:
        for key in connections.keys():
            connections[key].close()
            del connections[key]

def test_docs():
    """Tests the openmdao sphinx documentation.  
    A console script (openmdao_testdocs) calls this.
    This forces a build of the docs before testing.
    """
    branchdir, docdir, bindir =_get_dirnames()
    # force a new build before testing
    build_docs()
    sphinx.main(argv=['-P', '-b', 'doctest', '-d', 
                      os.path.join(docdir, '_build', 'doctrees'), 
                      docdir, os.path.join(docdir, '_build', 'html')])

    
def _get_border_line(numcols, colwidths, char):
    parts = []
    for i in range(numcols):
        parts.append(char*colwidths[i])
        parts.append(' ')
    parts.append('\n')
    return ''.join(parts)

def _get_table_cell( data, colwidth):
    return data+' '*(colwidth-len(data))
    
def _make_license_table(docdir, reqs=None):
    """
    Generates a file in docs/licenses/licenses_table.rst that
    contains a restructured text table with the name, license, and home-page of
    all distributions that openmdao depends on.
    """
    meta_names = ['name', 'version', 'license','home-page']
    headers = ['**Distribs Used by OpenMDAO**',
               '**Version**',
               '**License**',
               '**Link**']
    numcols = len(meta_names)
    data_templates = ["%s", "%s", "%s", "%s"]
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
    for projname,meta in metadict.items():
        for i,name in enumerate(meta_names):
            try:
                meta[name] = data_templates[i] % str(meta[name])
            except KeyError:
                meta[name] = 'UNKNOWN'
        if meta['name'] == 'UNKNOWN':
            meta['name'] = projname
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
    
def _compare_traits_path(x, y):
    if os.path.basename(x) > os.path.basename(y):
        return 1
    elif os.path.basename(x) < os.path.basename(y):
        return -1
    else:
        return 0
    

if __name__ == "__main__": #pragma: no cover
    build_docs()


