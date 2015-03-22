import sys
import os.path
import webbrowser
import tempfile
import tarfile
import shutil
import traceback
import requests
import base64

import urllib2
try:
    import simplejson as json
except ImportError:
    import json
import pprint
import StringIO
from ConfigParser import SafeConfigParser, NoSectionError, NoOptionError
from argparse import ArgumentParser, RawDescriptionHelpFormatter
from subprocess import call, check_call, STDOUT
import fnmatch

from collections import OrderedDict

from setuptools import find_packages
from pkg_resources import WorkingSet, Requirement, resource_stream

from openmdao.main.factorymanager import get_available_types, plugin_groups
from openmdao.util.fileutil import build_directory, find_files, \
                                   get_ancestor_dir, find_module
from openmdao.util.dep import PythonSourceTreeAnalyser
from openmdao.util.dumpdistmeta import get_metadata
from openmdao.util.git import download_github_tar
from openmdao.util.view_docs import view_docs
from openmdao.main import __version__

#from sphinx.setup_command import BuildDoc
import sphinx


def _load_templates():
    ''' Reads templates from files in the plugin_templates directory.

    conf.py:
      This is the template for the file that Sphinx uses to configure itself.
      It's intended to match the conf.py for the OpenMDAO docs, so if those
      change, this may need to be updated.

    index.rst
      Template for the top level file in the Sphinx docs for the plugin.

    usage.rst
      Template for the file where the user may add specific usage documentation
      for the plugin.

    setup.py
      Template for the file that packages and install the plugin using
      setuptools.

    MANIFEST.in
      Template for the file that tells setuptools/distutils what extra data
      files to include in the distribution for the plugin.

    README.txt
      Template for the README.txt file.

    setup.cfg
      Template for the setup configuration file, where all of the user
      supplied metadata is located.  This file may be hand edited by the
      plugin developer.

    '''

    # There are a number of string templates that are used to produce various
    # files within the plugin distribution. These templates are stored in the
    # templates dict, with the key being the name of the file that the
    # template corresponds to.
    templates = {}

    for item in ['index.rst', 'usage.rst', 'MANIFEST.in',
                 'README.txt', 'setup.cfg']:

        infile = resource_stream(__name__,
                                 os.path.join('plugin_templates', item))

        templates[item] = infile.read()
        infile.close()

    infile = resource_stream(__name__,
                             os.path.join('plugin_templates', 'setup_py_template'))

    templates['setup.py'] = infile.read()
    infile.close()

    infile = resource_stream(__name__,
                             os.path.join('plugin_templates', 'conf_py_template'))

    templates['conf.py'] = infile.read()
    infile.close()

    # This dict contains string templates corresponding to skeleton python
    # source files for each of the recognized plugin types.

    # TODO: These should be updated to reflect best practices because most
    # plugin developers will start with these when they create new plugins.
    class_templates = {}

    for item in ['openmdao.component', 'openmdao.driver', 'openmdao.variable',
                 'openmdao.surrogatemodel']:

        infile = resource_stream(__name__,
                                 os.path.join('plugin_templates', item))

        class_templates[item] = infile.read()
        infile.close()

    infile = resource_stream(__name__,
                             os.path.join('plugin_templates', 'test_template'))

    test_template = infile.read()
    infile.close()

    return templates, class_templates, test_template


def _get_srcdocs(destdir, name, srcdir='src'):
    """ Return RST for source docs. """
    startdir = os.getcwd()
    srcdir = os.path.join(destdir, srcdir)
    if os.path.exists(srcdir):
        os.chdir(srcdir)
        try:
            srcmods = _get_src_modules('.',
                                       dirpred=lambda d: not d.startswith('_') and d not in ['docs'])
        finally:
            os.chdir(startdir)
    else:
        srcmods = ["%s.%s" % (name, name)]

    contents = [
        """
.. _%s_src_label:


====================
Source Documentation
====================

        """ % name
    ]

    for mod in sorted(srcmods):
        pkgfile = '%s.py' % mod
        pkg, dot, name = mod.rpartition('.')
        pyfile = '%s.py' % name
        underline = '-'*len(pyfile)
        contents.append("""
.. index:: %s

.. _%s:

%s
%s

.. automodule:: %s
   :members:
   :undoc-members:
   :show-inheritance:

        """ % (pyfile, pkgfile, pyfile, underline, mod))

    return ''.join(contents)


def _get_pkgdocs(cfg):
    """Return a string in reST format that contains the metadata
    for the package.

    cfg: ConfigParser
        ConfigParser object used to read the setup.cfg file.
    """
    lines = ['\n',
             '================\n',
             'Package Metadata\n',
             '================\n',
             '\n']

    metadata = {}
    if cfg.has_section('metadata'):
        metadata.update(dict([item for item in cfg.items('metadata')]))
    if cfg.has_section('openmdao'):
        metadata.update(dict([item for item in cfg.items('openmdao')]))

    tuplist = list(metadata.items())
    tuplist.sort()
    for key, value in tuplist:
        if value.strip():
            if '\n' in value:
                lines.append("- **%s**:: \n\n" % key)
                for v in [vv.strip() for vv in value.split('\n')]:
                    if v:
                        lines.append("    %s\n" % v)
                lines.append('\n')
            elif value != 'UNKNOWN':
                lines.append("- **%s:** %s\n\n" % (key, value))

    return ''.join(lines)


def _get_setup_options(distdir, metadata, srcdir='src'):
    """ Return dictionary of setup options. """
    # a set of names of variables that are supposed to be lists
    lists = set([
        'keywords',
        'install_requires',
        'packages',
        'classifiers',
    ])

    # mapping of new metadata names to old ones
    mapping = {
        'name': 'name',
        'version': 'version',
        'keywords': 'keywords',
        'summary': 'description',
        'description': 'long_description',
        'home-page': 'url',
        'download-url': 'download_url',
        'author': 'author',
        'author-email': 'author_email',
        'maintainer': 'maintainer',
        'maintainer-email': 'maintainer_email',
        'license': 'license',
        'classifier': 'classifiers',
        'requires-dist': 'install_requires',
        'entry_points': 'entry_points',
        #'py_modules': 'py_modules',
        'packages': 'packages',
    }

    # populate the package data with sphinx docs
    # we have to list all of the files because setuptools doesn't
    # handle nested directories very well
    pkgdir = os.path.abspath(os.path.join(distdir, srcdir, metadata['name']))
    plen = len(pkgdir)+1
    sphinxdir = os.path.join(pkgdir, 'sphinx_build', 'html')
    testdir = os.path.join(pkgdir, 'test')
    pkglist = list(find_files(sphinxdir))
    pkglist.extend(list(find_files(testdir, exclude="*.py[co]")))
    pkglist = [p[plen:] for p in pkglist]
    setup_options = {
        #'packages': [metadata['name']],
        'package_data': {
            metadata['name']: pkglist  # [
            #'sphinx_build/html/*.*',
            #'sphinx_build/html/_modules/*',
            #'sphinx_build/html/_sources/*',
            #'sphinx_build/html/_static/*',
            #]
        },
        'package_dir': {'': srcdir},
        'zip_safe': False,
        'include_package_data': True,
    }

    for key, val in metadata.items():
        if key in mapping:
            if isinstance(val, basestring):
                if mapping[key] in lists:
                    val = [p.strip() for p in val.split('\n') if p.strip()]
                else:
                    val = val.strip()
            setup_options[mapping[key]] = val

    return setup_options


def _pretty(obj):
    """ Return pretty-printed `obj`. """
    sio = StringIO.StringIO()
    pprint.pprint(obj, sio)
    return sio.getvalue()


def _get_py_files(distdir, pred=None, dirpred=None):
    if pred is None:
        def pred(fname):
            parts = fname.split(os.sep)
            if parts[-1] in ['setup.py', '__init__.py'] or 'test' in parts:
                return False
            return fname.endswith('.py')
    return list(find_files(distdir, match=pred, dirmatch=dirpred))


def _get_src_modules(topdir, pred=None, dirpred=None):
    topdir = os.path.abspath(os.path.expandvars(os.path.expanduser(topdir)))
    pyfiles = _get_py_files(topdir, pred, dirpred)
    noexts = [os.path.splitext(f)[0] for f in pyfiles]
    rel = [f[len(topdir)+1:] for f in noexts]
    return ['.'.join(f.split(os.sep)) for f in rel]

def _get_dirs(start):
    dirs = []
    for root, dirlist, filelist in os.walk(start):
        newdlist = []
        for d in dirlist:
            if d.startswith('.') or d.endswith('.egg-info') or \
               d in ['docs', 'build', 'dists', 'sphinx_build']:
                continue
            newdlist.append(d)
        dirlist[:] = newdlist
        dirs.extend([os.path.join(root[len(start)+1:], d) for d in dirlist])
    return dirs

def _get_template_options(distdir, cfg, **kwargs):
    """ Return dictionary of options for template substitution. """
    if cfg.has_section('metadata'):
        metadata = dict([item for item in cfg.items('metadata')])
    else:
        metadata = {}
    if cfg.has_section('openmdao'):
        openmdao_metadata = dict([item for item in cfg.items('openmdao')])
    else:
        openmdao_metadata = {}

    if 'static_path' not in openmdao_metadata:
        openmdao_metadata['static_path'] = ''

    if 'packages' in kwargs:
        metadata['packages'] = kwargs['packages']
    else:
        metadata['packages'] = [metadata['name']]

    setup_options = _get_setup_options(distdir, metadata,
                                       srcdir=kwargs.get('srcdir', 'src'))

    template_options = {
        'copyright': '',
        'summary': '',
        'setup_options': _pretty(setup_options),
        'add_to_sys_path': _get_dirs(distdir),
    }

    template_options.update(setup_options)
    template_options.update(openmdao_metadata)
    template_options.update(kwargs)

    name = template_options['name']
    version = template_options['version']

    template_options.setdefault('release', version)
    template_options.setdefault('title_marker',
                                '='*(len(name)+len(' Documentation')))

    return template_options


def plugin_quickstart(parser, options, args=None):
    """A command-line script (plugin quickstart) points to this.  It generates a
    directory structure for an openmdao plugin package along with Sphinx docs.

    usage: plugin quickstart <dist_name> [-v <version>] [-d <dest_dir>] [-g <plugin_group>] [-c class_name]

    """
    if args:
        print_sub_help(parser, 'quickstart')
        return -1

    name = options.dist_name
    if options.classname:
        classname = options.classname
    else:
        classname = "%s%s" % ((name.upper())[0], name[1:])
    version = options.version

    options.dest = os.path.abspath(os.path.expandvars(os.path.expanduser(options.dest)))
    if not options.group.startswith('openmdao.'):
        options.group = 'openmdao.'+options.group

    templates, class_templates, test_template = _load_templates()

    startdir = os.getcwd()
    try:
        os.chdir(options.dest)

        if os.path.exists(name):
            raise OSError("Can't create directory '%s' because it already"
                          " exists." % os.path.join(options.dest, name))

        cfg = SafeConfigParser(dict_type=OrderedDict)
        stream = StringIO.StringIO(templates['setup.cfg'] % {'name': name,
                                                             'version': version})
        cfg.readfp(stream, 'setup.cfg')
        cfgcontents = StringIO.StringIO()
        cfg.write(cfgcontents)

        template_options = \
            _get_template_options(os.path.join(options.dest, name),
                                  cfg, classname=classname)

        template_options['srcmod'] = name

        dirstruct = {
            name: {
                'setup.py': templates['setup.py'] % template_options,
                'setup.cfg': cfgcontents.getvalue(),
                'MANIFEST.in': templates['MANIFEST.in'] % template_options,
                'README.txt': templates['README.txt'] % template_options,
                'src': {
                    name: {
                        '__init__.py': '',  # 'from %s import %s\n' % (name,classname),
                        '%s.py' % name: class_templates[options.group] % template_options,
                        'test': {
                                'test_%s.py' % name: test_template % template_options,
                                '__init__.py': """ """
                        },
                    },
                },
                'docs': {
                    'conf.py': templates['conf.py'] % template_options,
                    'index.rst': templates['index.rst'] % template_options,
                    'srcdocs.rst': _get_srcdocs(options.dest, name),
                    'pkgdocs.rst': _get_pkgdocs(cfg),
                    'usage.rst': templates['usage.rst'] % template_options,
                    '_static': {},
                },

            },
        }

        build_directory(dirstruct)

    finally:
        os.chdir(startdir)

    return 0


def _verify_dist_dir(dpath):
    """Try to make sure that the directory we've been pointed to actually
    contains a distribution.
    """
    if not os.path.isdir(dpath):
        raise IOError("directory '%s' does not exist" % dpath)

    expected = ['docs', 'setup.py', 'setup.cfg', 'MANIFEST.in',
                os.path.join('docs', 'conf.py'),
                os.path.join('docs', 'index.rst'),
                os.path.join('docs', 'srcdocs.rst')]
    for fname in expected:
        if not os.path.exists(os.path.join(dpath, fname)):
            raise IOError("directory '%s' does not contain '%s'" %
                          (dpath, fname))


_EXCL_SET = set(['test', 'docs', 'sphinx_build', '_downloads'])


def _exclude_funct(path):
    return len(_EXCL_SET.intersection(path.split(os.sep))) > 0


#
# FIXME: this still needs some work, but for testing purposes it's ok for now
#
def find_all_plugins(searchdir):
    """Return a dict containing lists of each plugin type found, keyed by
    plugin group name, e.g., openmdao.component, openmdao.variable, etc.
    """
    dct = {}
    psta = PythonSourceTreeAnalyser(searchdir, exclude=_exclude_funct)

    for key, lst in plugin_groups.items():
        epset = set(psta.find_inheritors(lst[0]))
        if epset:
            dct[key] = epset
    return dct


def _get_entry_points(startdir):
    """ Return formatted list of entry points. """
    plugins = find_all_plugins(startdir)
    entrypoints = StringIO.StringIO()
    for key, val in plugins.items():
        epts = []
        for v in val:
            if v.startswith('openmdao.'):
                continue
            mod, cname = v.rsplit('.', 1)
            epts.append('%s.%s=%s:%s' % (mod, cname, mod, cname))
        if epts:
            entrypoints.write("\n[%s]\n" % key)
            for ept in epts:
                entrypoints.write("%s\n" % ept)

    return entrypoints.getvalue()


def plugin_makedist(parser, options, args=None, capture=None, srcdir='src'):
    """A command-line script (plugin makedist) points to this.  It creates a
    source distribution containing Sphinx documentation for the specified
    distribution directory.  If no directory is specified, the current directory
    is assumed.

    usage: plugin makedist [dist_dir_path]

    """
    if args:
        print_sub_help(parser, 'makedist')
        return -1

    dist_dir = os.path.abspath(os.path.expandvars(os.path.expanduser(options.dist_dir_path)))
    _verify_dist_dir(dist_dir)

    startdir = os.getcwd()
    os.chdir(dist_dir)

    templates, class_templates, test_template = _load_templates()

    try:
        plugin_build_docs(parser, options)
        cfg = SafeConfigParser(dict_type=OrderedDict)
        cfg.readfp(open('setup.cfg', 'r'), 'setup.cfg')

        print "collecting entry point information..."
        cfg.set('metadata', 'entry_points', _get_entry_points(srcdir))

        template_options = _get_template_options(options.dist_dir_path, cfg,
                                                 packages=find_packages(srcdir))

        dirstruct = {
            'setup.py': templates['setup.py'] % template_options,
        }

        name = cfg.get('metadata', 'name')
        version = cfg.get('metadata', 'version')

        if sys.platform == 'win32':  # pragma no cover
            disttar = "%s-%s.zip" % (name, version)
        else:
            disttar = "%s-%s.tar.gz" % (name, version)
        disttarpath = os.path.join(startdir, disttar)
        if os.path.exists(disttarpath):
            print "Removing existing distribution %s" % disttar
            os.remove(disttarpath)

        build_directory(dirstruct, force=True)

        cmdargs = [sys.executable, 'setup.py', 'sdist', '-d', startdir]
        if capture:
            stdout = open(capture, 'w')
            stderr = STDOUT
        else:  # pragma no cover
            stdout = None
            stderr = None
        try:
            retcode = call(cmdargs, stdout=stdout, stderr=stderr)
        finally:
            if stdout is not None:
                stdout.close()
        if retcode:
            cmd = ' '.join(cmdargs)
            sys.stderr.write("\nERROR: command '%s' returned error code: %s\n"
                             % (cmd, retcode))
            return retcode
    finally:
        os.chdir(startdir)

    if os.path.exists(disttar):
        print "Created distribution %s" % disttar
        return 0
    else:
        sys.stderr.write("\nERROR: failed to make distribution %s" % disttar)
        return -1


# This brings up a browser window which can be a problem during testing.
def plugin_docs(parser, options, args=None):  # pragma no cover
    """A command-line script (plugin docs) points to this. It brings up
    the Sphinx documentation for the named plugin in a browser.
    """
    if args:
        print_sub_help(parser, 'docs')
        return -1

    if options.plugin_dist_name is None:
        view_docs(options.browser)
    else:
        url = find_docs_url(options.plugin_dist_name)
        wb = webbrowser.get(options.browser)
        wb.open(url)


def find_docs_url(plugin_name=None, build_if_needed=True):
    """Returns a url for the Sphinx docs for the named plugin.
    The plugin must be importable in the current environment.

    plugin_name: str
        Name of the plugin distribution, module, or class.
    """
    parts = plugin_name.split('.')

    if len(parts) == 1:
        # assume it's a class name and try to find unambiguous module
        modname = None
        # loop over available types to find a class name that matches
        for name, version in get_available_types():
            mname, cname = name.rsplit('.', 1)
            if cname == plugin_name:
                if modname and modname != mname:
                    raise RuntimeError("Can't determine module for class '%s'"
                                       " unambiguously. found in %s"
                                       % (cname, [mname, modname]))
                modname = mname
                parts = modname.split('.')

        if modname is None:
            # didn't find a class, so assume plugin_name is a dist name
            parts = [plugin_name, plugin_name]

    for i in range(len(parts)-1):
        mname = '.'.join(parts[:len(parts)-i])
        try:
            __import__(mname)
            mod = sys.modules[mname]
            modname = mname
            modfile = os.path.abspath(mod.__file__)
            break
        except ImportError:
            # we may be able to locate the docs even if the import fails
            modfile = find_module(mname)
            modname = mname
            if modfile:
                break
    else:
        # Possibly something in contrib that's a directory.
        try:
            __import__(plugin_name)
            mod = sys.modules[plugin_name]
            modname = plugin_name
            modfile = os.path.abspath(mod.__file__)
        except ImportError:
            raise RuntimeError("Can't locate package/module '%s'" % plugin_name)

    url = 'file://'
    if modname.startswith('openmdao.'):  # lookup in builtin docs
        import openmdao.main
        fparts = mod.__file__.split(os.sep)
        pkg = '.'.join(modname.split('.')[:2])
        anchorpath = '/'.join(['srcdocs', 'packages',
                               '%s.html#module-%s' % (pkg, modname)])
        if any([p.endswith('.egg') and p.startswith('openmdao.')
                for p in fparts]):
            # this is a release version, so use docs packaged with openmdao.main
            htmldir = os.path.join(os.path.dirname(openmdao.main.__file__), "docs")
        else:  # it's a developer version, so use locally built docs
            htmldir = os.path.join(get_ancestor_dir(sys.executable, 3), 'docs',
                                   '_build', 'html')
            if not os.path.isfile(os.path.join(htmldir, 'index.html')) and build_if_needed:
                #make sure the local docs are built
                print "local docs not found.\nbuilding them now...\n"
                check_call(['openmdao', 'build_docs'])
        url += os.path.join(htmldir, anchorpath)
    else:
        url += os.path.join(os.path.dirname(modfile),
                           'sphinx_build', 'html', 'index.html')

    url = url.replace('\\', '/')
    return url


def plugin_install(parser, options, args=None, capture=None):
    """A command-line script (plugin install) points to this. It installs
    the specified plugin distribution into the current environment.

    """
    if args:
        print_sub_help(parser, 'install')
        return -1

    if options.all:
        if options.owner:
            owner = options.owner
        else:
            owner = 'OpenMDAO-Plugins'

        #Get everything from OpenMDAO-Plugins
        plugin_url = "https://api.github.com/users/{owner}/repos?type=public"
        plugin_url = plugin_url.format(owner=owner)
        custom_github_plugins = []
        plugin_page = urllib2.urlopen(plugin_url)
        for line in plugin_page.fp:
           text = json.loads(line)
           for item in sorted(text):
               custom_github_plugins.append(item['name'])

        for plugin in custom_github_plugins:
            try:
                print "Installing plugin:", plugin
                _github_install(plugin, options.findlinks, owner)
            except Exception:
                msg = "The plugin failed to install. You should notify the plugin maintainer \
                with the error message below."
                traceback.print_exc()

    elif options.owner:
        #Get specified plugin from owner
        _github_install(options.dist_name, options.findlinks, options.owner)


    else:  # Install plugin from local file or directory
        develop = False

        if not options.dist_name:
            print "installing distribution from current directory as a 'develop' egg"
            develop = True

        if develop:
            cmdargs = [sys.executable, 'setup.py', 'develop', '-N']
        else:
            cmdargs = ['easy_install', '-f', options.findlinks, options.dist_name]

        cmd = ' '.join(cmdargs)
        if capture:
            stdout = open(capture, 'w')
            stderr = STDOUT
        else:  # pragma no cover
            stdout = None
            stderr = None
        try:
            retcode = call(cmdargs, stdout=stdout, stderr=stderr)
        finally:
            if stdout is not None:
                stdout.close()
        if retcode:
            sys.stderr.write("\nERROR: command '%s' returned error code: %s\n"
                             % (cmd, retcode))
            return -1

    return 0

def _github_install(dist_name, findLinks, owner):
    # Get plugin from github.
    #FIXME: this should support all valid version syntax (>=, <=, etc.)

    pieces = dist_name.split('==')
    name = pieces[0]
    url = 'https://api.github.com/repos/{owner}/{repo}/tarball/{ref}'

    # User specified version using easy_install style ("plugin==version")
    if len(pieces) > 1:
        version = pieces[1]

    # Get most recent version from our tag list
    else:
        tags_url = 'https://api.github.com/repos/%s/%s/tags' % (owner, name)
        try:
            resp = urllib2.urlopen(tags_url)
        except urllib2.HTTPError:
            print "\nERROR: plugin named '%s' not found in %s" % (name, owner)
            return -1

        for line in resp.fp:
            text = json.loads(line)

            tags = []
            for item in text:
                tags.append(item['name'])
        try:
            tags.sort(key=lambda s: map(int, s.split('.')))
        except ValueError:
            print "\nERROR: the releases for the plugin named '%s' have" \
                  " not been tagged correctly for installation." % name
            print "You may want to contact the repository owner"
            return -1

        if not tags:
            print "\nPlugin named '%s' has no tagged releases." % name
            print "You may want to contact the repository owner to create a tag."
            print "Grabbing the most recent version of default branch instead..."

        if tags:
            version = tags[-1]

        else:
            tarball_url = 'https://api.github.com/repos/{owner}/{repo}/tarball'
            tarball_url = tarball_url.format(owner=owner, repo=dist_name)

            resp = requests.get(tarball_url, allow_redirects=False)

            if resp.status_code == 302:
                version = resp.headers['location'].split('/')[-1]

            else:
                print "\nERROR: plugin named '%s' not found in %s" % (name, owner)
                return -1

    print url.format(
        owner=owner,
        repo=name,
        ref=version
    )

    build_docs_and_install(owner, name, version, findLinks)

def _bld_sdist_and_install(deps=True):
    check_call([sys.executable, 'setup.py', 'sdist', '-d', '.'])

    if sys.platform.startswith('win'):
        tars = fnmatch.filter(os.listdir('.'), "*.zip")
    else:
        tars = fnmatch.filter(os.listdir('.'), "*.tar.gz")
    if len(tars) != 1:
        raise RuntimeError("should have found a single archive file,"
                           " but found %s instead" % tars)

    if deps:
        opts = '-NZ'
    else:
        opts = '-Z'
    check_call(['easy_install', opts, tars[0]])

    return tars[0]

# This requires Internet connectivity to github.
def build_docs_and_install(owner, name, version, findlinks):  # pragma no cover
    tdir = tempfile.mkdtemp()
    startdir = os.getcwd()
    os.chdir(tdir)
    try:
        tarpath = download_github_tar(owner, name, version)

        # extract the repo tar file
        tar = tarfile.open(tarpath)
        tar.extractall()
        tar.close()

        files = os.listdir('.')
        files.remove(os.path.basename(tarpath))
        if len(files) != 1:
            raise RuntimeError("after untarring, found multiple directories: %s"
                               % files)

        os.chdir(files[0])  # should be in distrib directory now

        cfg = SafeConfigParser(dict_type=OrderedDict)

        try:
            cfg.readfp(open('setup.cfg', 'r'), 'setup.cfg')

        except IOError as io_error:
            raise IOError, \
                "OpenMDAO plugins must have a setup.cfg: {}".format(io_error), \
                sys.exc_info()[2]

        try:
            reqs = cfg.get('metadata', 'requires-dist').strip()
            reqs = reqs.replace(',', ' ')
            reqs = [n.strip() for n in reqs.split()]

            try:
                flinks = cfg.get('easy_install', 'find_links').strip()
                flinks = flinks.split('\n')
                flinks = [n.strip() for n in flinks]

                flinks.append(findlinks)

                findlinks = ' '.join(flinks)

            except (NoSectionError, NoOptionError):
                pass

        except NoOptionError:
            # couldn't find requires-dist in setup.cfg, so
            # create an sdist so we can query metadata for distrib dependencies
            tarname = _bld_sdist_and_install(deps=False)

            # now find any dependencies
            metadict = get_metadata(tarname)
            reqs = metadict.get('requires', [])

        # install dependencies (some may be needed by sphinx)
        ws = WorkingSet()
        for r in reqs:
            print "Installing dependency '%s'" % r
            req = Requirement.parse(r)
            dist = ws.find(req)
            if dist is None:
                try:
                    check_call(['easy_install', '-Z', '-f', findlinks, r])
                except Exception:
                    traceback.print_exc()

        # build sphinx docs
        check_call(['plugin', 'build_docs', files[0]])

        # make a new sdist with docs in it and install it
        tarname = _bld_sdist_and_install()
    finally:
        os.chdir(startdir)
        shutil.rmtree(tdir, ignore_errors=True)


def _plugin_build_docs(destdir, cfg, src='src'):
    """Builds the Sphinx docs for the plugin distribution, assuming it has
    a structure like the one created by plugin quickstart.
    """
    name = cfg.get('metadata', 'name')
    version = cfg.get('metadata', 'version')

    docdir = os.path.join(destdir, 'docs')
    srcdir = os.path.abspath(os.path.join(destdir, src))

    sphinx.main(argv=['', '-E', '-a', '-b', 'html',
                      '-Dversion=%s' % version,
                      '-Drelease=%s' % version,
                      '-d', os.path.join(srcdir, name, 'sphinx_build', 'doctrees'),
                      docdir,
                      os.path.join(srcdir, name, 'sphinx_build', 'html')])


def plugin_build_docs(parser, options, args=None):
    """A command-line script (plugin build_docs) points to this.  It builds the
    Sphinx documentation for the specified distribution directory.
    If no directory is specified, the current directory is assumed.

    usage: plugin build_docs [dist_dir_path]

    """
    if args is not None and len(args) > 1:
        print_sub_help(parser, 'build_docs')
        return -1

    if args:
        dist_dir = args[0]
    else:
        dist_dir = '.'
    dist_dir = os.path.abspath(os.path.expandvars(os.path.expanduser(dist_dir)))

    _verify_dist_dir(dist_dir)

    #pfiles = fnmatch.filter(os.listdir(options.srcdir), '*.py')
    #if not pfiles:
    #    options.srcdir = dist_dir

    cfgfile = os.path.join(dist_dir, 'setup.cfg')
    cfg = SafeConfigParser(dict_type=OrderedDict)
    cfg.readfp(open(cfgfile, 'r'), cfgfile)

    cfg.set('metadata', 'entry_points',
            _get_entry_points(os.path.join(dist_dir, options.srcdir)))

    templates, class_templates, test_template = _load_templates()
    template_options = _get_template_options(dist_dir, cfg, srcdir=options.srcdir)

    dirstruct = {
        'docs': {
            'conf.py': templates['conf.py'] % template_options,
            'pkgdocs.rst': _get_pkgdocs(cfg),
            'srcdocs.rst': _get_srcdocs(dist_dir,
                                        template_options['name'],
                                        srcdir=options.srcdir),
        },
    }

    build_directory(dirstruct, force=True, topdir=dist_dir)
    _plugin_build_docs(dist_dir, cfg, src=options.srcdir)
    return 0


def plugin_list(parser, options, args=None):
    """ List GitHub/external/built-in plugins. """
    if args:
        print_sub_help(parser, 'list')
        return -1

    # Requires Internet to access github.
    if options.owner:  # pragma no cover

        _list_github_plugins(options.owner)
        return 0

    groups = []
    for group in options.groups:
        if not group.startswith('openmdao.'):
            group = 'openmdao.'+group
        groups.append(group)

    show_all = (options.external == options.builtin)
    if show_all:
        title_type = ''
    elif options.external:
        title_type = 'external'
    else:
        title_type = 'built-in'

    title_groups = ','.join([g.split('.')[1] for g in groups])
    parts = title_groups.rsplit(',', 1)
    if len(parts) > 1:
        title_groups = ' and '.join(parts)

    if not groups:
        groups = None
    all_types = get_available_types(groups)

    plugins = set()
    for type in all_types:
        if show_all:
            plugins.add((type[0], type[1]['version']))
        else:
            name = type[0].split('.')[0]
            if name == 'openmdao':
                if options.builtin:
                    plugins.add((type[0], type[1]['version']))
            else:
                if options.external:
                    plugins.add((type[0], type[1]['version']))

    title = "Installed %s %s plugins" % (title_type, title_groups)
    title = title.replace('  ', ' ')
    under = '-'*len(title)
    print ""
    print title
    print under
    print ""
    for plugin in sorted(plugins):
        print plugin[0], plugin[1]

    print "\n"

    return 0


def print_sub_help(parser, subname):
    """Prints a usage message for the given subparser name."""
    for obj in parser._subparsers._actions:
        if obj.dest != 'help':
            obj.choices[subname].print_help()
            return
    raise NameError("unknown subparser name '%s'" % subname)


# Requires Internet to access github.
def _list_github_plugins(owner):  # pragma no cover
    url = 'https://api.github.com/users/{owner}/repos?type=public'
    url = url.format(owner=owner)

    if owner != "OpenMDAO-Plugins":
        msg = "This is a listing of {user}'s repositories. " \
        "There is no guarantee that these are OpenMDAO plugins.\n\n" \
        "Please contact {user} to get an official " \
        "listing of their OpenMDAO plugins."

        print msg.format(user=owner)

    print "\nAvailable plugin distributions"
    print "==============================\n"

    resp = requests.get(url, headers={'Connection':'close'})
    items = None

    if resp.ok:
        items = sorted(resp.json())

        for item in items:
            print '%20s -- %s' % (item['name'], item['description'])
        print '\n'

def _get_plugin_parser():
    """Sets up the plugin arg parser and all of its subcommand parsers."""

    top_parser = ArgumentParser()
    subparsers = top_parser.add_subparsers(title='commands')

    parser = subparsers.add_parser('list',
                        help="List installed plugins, or list plugins " \
                             "available on github.")

    parser.usage = "plugin list [options]"
    parser.add_argument("--github",
                        help='List plugin repos owned by OWNER on github.',
                        action='store',
                        nargs='?',
                        type=str,
                        dest='owner',
                        const='OpenMDAO-Plugins')

    parser.add_argument("-b", "--builtin",
                        help='List all installed plugins that are part of the'
                             ' OpenMDAO distribution',
                        action='store_true')

    parser.add_argument("-e", "--external",
                        help='List all installed plugins that are not part of'
                             ' the OpenMDAO distribution',
                        action='store_true')

    parser.add_argument("-g", "--group",
                        action="append",
                        type=str,
                        dest='groups',
                        default=[],
                        choices=[p.split('.', 1)[1]
                                 for p in plugin_groups.keys()],
                        help="List plugins of a specified type. "
                             "Type should be one of the listed options.")

    parser.set_defaults(func=plugin_list)

    parser = subparsers.add_parser('install',
                                   help="install an OpenMDAO plugin into the"
                                        " current environment",
                                   description="Install selected plugin or plugins either locally or from github.",
                                   formatter_class=RawDescriptionHelpFormatter)

    parser.usage = "plugin install [dist_name] [options] \n"

    parser.epilog = "example: get the plugin pyopt_driver from github \n" \
                    ">plugin install pyopt_driver --github\n\n" \
                    "example: install all OpenMDAO-compatible plugins from GithubMember's repo.\n" \
                    ">plugin install --github GithubMember --all"

    #can't do a single component and --all in same command, make them m.e.
    group = parser.add_mutually_exclusive_group()

    group.add_argument('dist_name',
                        help='name of a single plugin distribution.  cannot be used with --all.'
                             '(defaults to distrib found in current dir)',
                        nargs='?')

    parser.add_argument("--github", action='store',  nargs='?', type=str, dest='owner',
                        const='OpenMDAO-Plugins',
                        help='Search for plugin repo owned by OWNER on github. '
                             'Defaults to \'OpenMDAO-Plugins\'' )

    parser.add_argument("-f", "--find-links", action="store", type=str,
                        dest='findlinks', default='http://openmdao.org/dists',
                        help="URL of find-links server. Defaults to "
                             "'http://openmdao.org/dists'")

    group.add_argument("--all",
                        help='Install all plugins owned by OWNER as specified '
                             'with --github.  Defaults to OpenMDAO-Plugins.',
                        action='store_true')

    parser.set_defaults(func=plugin_install)

    parser = subparsers.add_parser('build_docs',
                                   help="build sphinx doc files for a plugin")
    parser.usage = "plugin build_docs <dist_dir_path>"
    parser.add_argument('dist_dir_path', default='.',
                        help='path to distribution source directory')
    parser.add_argument("-s", "--srcdir", action="store", type=str,
                        dest='srcdir', default='src',
                        help="top directory in the distribution where python"
                             " source is located")
    parser.set_defaults(func=plugin_build_docs)

    parser = subparsers.add_parser('docs',
                                   help="display docs for a plugin")
    parser.usage = "plugin docs <plugin_dist_name>"

    parser.add_argument('plugin_dist_name', help='name of plugin distribution')
    parser.add_argument("-b", "--browser", action="store", type=str,
                        dest='browser', choices=webbrowser._browsers.keys(),
                        help="browser name")
    parser.set_defaults(func=plugin_docs)

    parser = subparsers.add_parser('quickstart', help="generate some skeleton"
                                                      " files for a plugin")
    parser.usage = "plugin quickstart <dist_name> [options]"
    parser.add_argument('dist_name', help='name of distribution')
    parser.add_argument("-v", "--version", action="store", type=str,
                        dest='version', default='0.1',
                        help="version id of the plugin (defaults to 0.1)")
    parser.add_argument("-c", "--class", action="store", type=str,
                        dest='classname', help="plugin class name")
    parser.add_argument("-d", "--dest", action="store", type=str, dest='dest',
                        default='.',
                        help="directory where new plugin directory will be"
                             " created (defaults to current dir)")
    parser.add_argument("-g", "--group", action="store", type=str, dest='group',
                        default='openmdao.component',
                        help="specify plugin group %s (defaults to"
                             " 'openmdao.component')" % plugin_groups.keys())
    parser.set_defaults(func=plugin_quickstart)

    parser = subparsers.add_parser('makedist', help="create a source"
                                                   " distribution for a plugin")
    parser.usage = "plugin makedist [dist_dir_path]"
    parser.add_argument('dist_dir_path', nargs='?',
                        default='.',
                        help='directory where plugin distribution is found'
                             ' (defaults to current dir')
    parser.add_argument("-s", "--srcdir", action="store", type=str,
                        dest='srcdir', default='src',
                        help="top directory in the distribution where python"
                             " source is located")
    parser.set_defaults(func=plugin_makedist)

    return top_parser


# Calls sys.exit().
def plugin():  # pragma no cover
    parser = _get_plugin_parser()
    options, args = parser.parse_known_args()
    sys.exit(options.func(parser, options, args))
