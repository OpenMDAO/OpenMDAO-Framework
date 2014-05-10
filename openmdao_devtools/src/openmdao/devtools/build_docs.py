import os
import os.path
import sys
import shutil
import fnmatch
import logging
from subprocess import Popen, PIPE, STDOUT
from pkg_resources import Requirement, working_set
import sphinx

from openmdao.main.plugin import print_sub_help
from openmdao.util.dumpdistmeta import get_dist_metadata
from openmdao.util.fileutil import get_ancestor_dir, onerror

# Specify modules and packages to be included in the OpenMDAO documentation here
srcmods = [
]

_is_release = False


def _include_pkg(name):
    if name.startswith('openmdao.'):
        if name.startswith('openmdao.examples'):
            return False
        if _is_release and name.startswith('openmdao.devtools'):
            return False
        return True
    return False


def get_openmdao_packages():
    return [d.project_name for d in working_set if _include_pkg(d.project_name)]


logger = logging.getLogger()


def get_rev_info():
    p = Popen('git describe --tags',
              stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
    out = p.communicate()[0].strip()
    tag, ncommits, commit = out.rsplit('-', 2)
    return tag, ncommits, commit


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

    rstfile = _get_rst_path(mod)
    if(rstfile):
        outfile.write('.. include:: %s\n' % rstfile)

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
        logging.error('no dist found for Requirement(%s)' % pkg)
    print >> outfile, 'Package %s' % pkg
    print >> outfile, underline*(len('Package ')+len(pkg))
    print >> outfile, '\n\n'

    __import__(pkg)
    mod = sys.modules[pkg]
    docs = mod.__doc__

    if docs:
        print >> outfile, docs, '\n'

    #excluding traits now since they need to be sorted separately
    #also excluding gui-related files, in case of non-gui build
    _names = list(_get_resource_files(dist,
                                    ['*__init__.py', '*setup.py', '*datatypes*.py',
                                     '*/main/zmq*.py', '*/main/tornado*.py',
                                     '*/gui/*/views.py', '*/gui/*/models.py',
                                     '*/gui/*/urls.py', '*/gui/*/admin.py'],
                                    ['*.py']))
    names = []
    for n in _names:
        parts = n.split('/')
        if parts[0] == 'openmdao' and parts[1] == 'test':
            if len(parts) > 2 and parts[2] != 'plugins':
                names.append(n)
        elif 'test' not in parts:
            names.append(n)

    names.sort()

    #wanted to sort traits separately based only on filenames despite differing paths
    traitz = list(_get_resource_files(dist,
                                      ['*__init__.py', '*setup.py', '*/test/*.py'],
                                      ['*/main/datatypes*.py', '*/lib/datatypes*.py']))
    sorted_traitz = sorted(traitz, cmp=_compare_traits_path)

    names.extend(sorted_traitz)

    exdirs = ['build', 'examples']

    oldheader = None
    newheader = None

    for name in names:
        if os.path.basename(name) == 'releaseinfo.py':
            continue

        for ex in exdirs:
            if name.startswith('%s/' % ex) or '/%s/' % ex in name:
                break
            else:
                x = name.split('/')
                #kind of dirty, but the other sections don't need api header.
                if os.path.basename(name) == 'api.py' and x[1] == 'lib':
                    newheader = 'api'
                if len(x) >= 4:
                    newheader =  x[2]
            if (oldheader != newheader):
                print >> outfile, '**%s**' % newheader.upper()
                print >> outfile, '_' * (4 + len(newheader)) + '\n'
                oldheader = newheader

        _mod_sphinx_info(name, outfile, show_undoc=show_undoc)


def _write_src_docs(branchdir, docdir):
    # first, clean up the old stuff, if any
    pkgdir = os.path.join(docdir, 'srcdocs', 'packages')
    moddir = os.path.join(docdir, 'srcdocs', 'modules')

    for name in os.listdir(pkgdir):
        if name != '.gitignore':
            os.remove(os.path.join(pkgdir, name))

    for name in os.listdir(moddir):
        if name != '.gitignore':
            os.remove(os.path.join(moddir, name))

    for pack in get_openmdao_packages():
        print 'creating autodoc file for %s' % pack
        with open(os.path.join(pkgdir, pack+'.rst'), 'w') as f:
            _pkg_sphinx_info(branchdir, pack, f, show_undoc=True, underline='-')

    for src in srcmods:
        with open(os.path.join(docdir, 'srcdocs', 'modules',
                              os.path.basename(src)+'.rst'), 'w') as f:
            logger.info('creating autodoc file for %s' % src)
            _mod_sphinx_info(os.path.basename(src), f)


def build_docs(parser=None, options=None, args=None):
    """A script (openmdao build_docs) points to this.  It generates the Sphinx
    documentation for openmdao.
    """
    global _is_release
    if args and parser:
        print_sub_help(parser, 'build_docs')
        return -1

    if options is not None and hasattr(options, 'version') and options.version:
        version = options.version
        shtitle = 'OpenMDAO Documentation v%s' % version
        _is_release = True
    else:
        _is_release = False
        try:
            tag, ncommits, commit = get_rev_info()
            version = "%s-%s-%s" % (tag, ncommits, commit)
            shtitle = 'OpenMDAO Documentation (%s commits after version %s)' % (ncommits, tag)
        except:
            # try to get commit id
            try:
                top = get_ancestor_dir(sys.executable, 3)
                if '-OpenMDAO-Framework-' in top:
                    commit = top.split('-')[-1]
                    version = "dev - commit id: %s" % commit
                    shtitle = "OpenMDAO Documentation (commit id %s)" % commit
                else:
                    raise RuntimeError("can't find commit id")
            except:
                version = "?-?-?"
                shtitle = "OpenMDAO Documentation (unknown revision)"

    branchdir, docdir, bindir = _get_dirnames()

    startdir = os.getcwd()
    if not os.path.isdir(docdir):
        raise RuntimeError('doc directory '+docdir+' not found')

    _write_src_docs(branchdir, docdir)
    _make_license_table(docdir)

    os.chdir(docdir)
    try:
        # make necessary directories
        if os.path.isdir(os.path.join('_build', 'html')):
            shutil.rmtree(os.path.join('_build', 'html'), onerror=onerror)
        if os.path.isdir(os.path.join('_build', 'doctrees')):
            shutil.rmtree(os.path.join('_build', 'doctrees'), onerror=onerror)
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
    """A script (openmdao docs) points to this. It just pops up a browser to
    view the openmdao Sphinx docs. If the docs are not already built, it
    builds them before viewing. But if the docs already exist, it's not smart enough
    to rebuild them if they've changed since the last build.
    """
    if not browser:
        for arg in sys.argv:
            if arg.startswith('--browser='):
                browser = arg.split('=')[-1].strip()

    branchdir, docdir, bindir = _get_dirnames()
    idxpath = os.path.join(docdir, '_build', 'html', 'index.html')
    if not os.path.isfile(idxpath):
        build_docs()

    import webbrowser
    idxpath = "file://"+idxpath
    wb = webbrowser.get(browser)
    wb.open(idxpath)


def test_docs(parser, options, args=None):
    """Tests the openmdao Sphinx documentation.
    A console script (openmdao test_docs) calls this.
    This forces a build of the docs before testing.
    """
    branchdir, docdir, bindir = _get_dirnames()
    # force a new build before testing
    build_docs(parser, options, args)
    return sphinx.main(argv=['-P', '-b', 'doctest', '-d',
                             os.path.join(docdir, '_build', 'doctrees'),
                             docdir, os.path.join(docdir, '_build', 'html')])

# make nose ignore this function
test_docs.__test__ = False


def _get_border_line(numcols, colwidths, char):
    parts = []
    for i in range(numcols):
        parts.append(char*colwidths[i])
        parts.append(' ')
    parts.append('\n')
    return ''.join(parts)


def _get_table_cell(data, colwidth):
    return data+' '*(colwidth-len(data))


def _make_license_table(docdir, reqs=None):
    """
    Generates a file in docs/licenses/licenses_table.rst that
    contains a restructured text table with the name, license, and home-page of
    all distributions that openmdao depends on.
    """
    meta_names = ['name', 'version', 'license', 'home-page']
    headers = ['**Distribs Used by OpenMDAO**',
               '**Version**',
               '**License**',
               '**Link**']
    numcols = len(meta_names)
    data_templates = ["%s", "%s", "%s", "%s"]
    col_spacer = ' '
    max_col_width = 80

    license_fname = os.path.join(docdir, 'licenses', 'licenses_table.txt')

    if reqs is None:
        reqs = [Requirement.parse(p) for p in get_openmdao_packages()]

    reqset = set(reqs)
    dists = set()
    done = set()
    while reqset:
        req = reqset.pop()
        if req.project_name not in done:
            done.add(req.project_name)
            dist = working_set.find(req)
            if dist is not None:
                dists.add(dist)
                reqset.update(dist.requires())

    metadict = {}
    for dist in dists:
        metadict[dist.project_name] = get_dist_metadata(dist)
    for projname, meta in metadict.items():
        for i, name in enumerate(meta_names):
            try:
                meta[name] = data_templates[i] % str(meta[name])
            except KeyError:
                meta[name] = 'UNKNOWN'
        if meta['name'] == 'UNKNOWN':
            meta['name'] = projname
    # figure out sizes of table columns
    colwidths = [len(s)+1 for s in headers]
    for i, name in enumerate(meta_names):
        sz = max([len(m[name]) for m in metadict.values()])+1
        sz = min(sz, max_col_width)
        colwidths[i] = max(colwidths[i], sz)

    with open(license_fname, 'wb') as outfile:
        # write header
        outfile.write(_get_border_line(numcols, colwidths, char='='))
        for i, header in enumerate(headers):
            outfile.write(header+' '*(colwidths[i]-len(header)))
            outfile.write(col_spacer)
        outfile.write('\n')
        outfile.write(_get_border_line(numcols, colwidths, char='='))

        # write table data
        tups = [(k, v) for k, v in metadict.items()]
        tups = sorted(tups, lambda x, y: cmp(x[0].lower(), y[0].lower()))
        for j, tup in enumerate(tups):
            for i, name in enumerate(meta_names):
                outfile.write(_get_table_cell(tup[1][name], colwidths[i]))
                outfile.write(col_spacer)
            outfile.write('\n')
            if j < len(tups) - 1:
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


def _get_rst_path(obj):
    """
    Finds accompanying documentation that exists for some .py files.
    """
    bindir = os.path.dirname(sys.executable)
    branchdir = os.path.dirname(os.path.dirname(bindir))
    writedir = os.path.join(branchdir, 'docs', 'srcdocs', 'packages')

    rstfile = os.path.basename(os.path.splitext(obj)[0] + ".rst")
    pyabs = os.path.join(branchdir, '.'.join(obj.split('/')[:2]), 'src', obj)
    textfilepath = os.path.join(os.path.dirname(pyabs), 'docs', rstfile)
    if os.path.isfile(textfilepath):
        #The sphinx include directive needs a relative path
        #to the text file, rel to the docs dir
        relpath = os.path.relpath(textfilepath, writedir)
        if (relpath):
            return relpath
