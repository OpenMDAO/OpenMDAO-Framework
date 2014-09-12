
import sys
import os
import shutil
import fnmatch
import atexit

from openmdao.devtools.utils import put_dir, repo_top, fabric_cleanup


def _push_release(release_dir, destination, obj, py='python'):
    """Take a directory containing release files (openmdao package
    distributions, install scripts, etc., and place the files in the
    proper locations on the server.

    release_dir: str
        where the release file are located

    destination: str
        the location where the release files are to be placed. It can be a URL
        or a local directory

    obj: _CommObj
        an object to wrap the behaviors of run, put, etc. so calls are the
        same for local or remote release areas

    py: str
        python version to use to build index files
    """
    files = os.listdir(release_dir)
    f = fnmatch.filter(files, 'go-openmdao-*.py')
    try:
        f.remove('go-openmdao-dev.py')
    except:
        pass
    if len(f) < 1:
        raise RuntimeError("can't find go-openmdao-*.py file in release directory")
    elif len(f) > 1:
        raise RuntimeError("more than one file in release dir matches 'go-openmdao-*.py'")
    script = f[0]

    # determine version from the form of the go-openmdao-?.?.py file
    version = os.path.splitext(script)[0].split('-', 2)[2]

    # the following will barf if the version already exists on the server
    obj.run('mkdir %s/downloads/%s' % (destination, version))
    obj.run('chmod 755 %s/downloads/%s' % (destination, version))

    # push new distribs to the server
    for f in os.listdir(release_dir):
        if (f.endswith('.tar.gz') and f != 'docs.tar.gz') or f.endswith('.egg'):
            obj.put(os.path.join(release_dir,f), '%s/dists/%s' % (destination, f))
            obj.run('chmod 644 %s/dists/%s' % (destination, f))

    obj.put(os.path.join(release_dir, 'go-openmdao-%s.py' % version),
        '%s/downloads/%s/go-openmdao-%s.py' % (destination, version, version))
    obj.run('chmod 755 %s/downloads/%s/go-openmdao-%s.py' % (destination, version, version))

    # put the docs on the server
    obj.put_dir(os.path.join(release_dir, 'docs'),
                '%s/downloads/%s/docs' % (destination, version))

    obj.put(os.path.join(repo_top(),'scripts','mkdlversionindex.py'),
            '%s/downloads/%s/mkdlversionindex.py' % (destination, version))

    obj.put(os.path.join(repo_top(),'scripts','mkegglistindex.py'),
            '%s/dists/mkegglistindex.py' % destination)

    obj.put(os.path.join(repo_top(),'scripts','mkdownloadindex.py'),
            '%s/downloads/mkdownloadindex.py' % destination)

    cdir = os.getcwd()

    # update the index.html for the version download directory on the server
    dpath = '%s/downloads/%s' % (destination, version)
    obj.run('cd %s && %s mkdlversionindex.py' % (dpath, py))

    os.chdir(cdir)

    # update the index.html for the dists directory on the server
    dpath = '%s/dists' % destination
    obj.run('cd %s && %s mkegglistindex.py' % (dpath, py))

    os.chdir(cdir)

    # update the 'latest' link
    obj.run('rm -f %s/downloads/latest' % destination)
    obj.run('ln -s -f %s %s/downloads/latest' % (version, destination))

    os.chdir(cdir)

    # update the index.html for the downloads directory on the server
    dpath = '%s/downloads' % destination
    obj.run('cd %s && %s mkdownloadindex.py' % (dpath, py))


def _setup_local_release_dir(dpath):
    dn = os.path.dirname
    topdir = dn(dn(dn(sys.executable)))
    os.makedirs(os.path.join(dpath, 'downloads', 'misc'))
    os.makedirs(os.path.join(dpath, 'dists'))
    shutil.copy(os.path.join(topdir,'scripts','mkdownloadindex.py'),
                dpath)

class _CommObj(object):
    pass

def push_release(parser, options):
    from fabric.api import run, local, put, settings
    atexit.register(fabric_cleanup, True)

    comm_obj = _CommObj()

    if options.releasedir is None:
        print "release directory was not specified"
        sys.exit(-1)

    destdir = options.destdir
    if destdir is None:
        if os.path.basename(options.releasedir).startswith('rel_'):
            destdir = os.path.join(os.path.dirname(options.releasedir),
                                   "release_%s" %
                                   os.path.basename(options.releasedir).split('_', 1)[1])
        else:
            print "destination directory not supplied"
            sys.exit(-1)

    if os.path.isdir(destdir):
        print "destination dir '%s' already exists" % destdir
        sys.exit(-1)

    if not os.path.isdir(options.releasedir):
        print "release directory %s not found" % options.releasedir
        sys.exit(-1)

    if not ('@' in destdir or ':' in destdir): # it's a local release test area
        if not os.path.isdir(destdir):
            _setup_local_release_dir(destdir)
        comm_obj.put = shutil.copy
        comm_obj.put_dir = shutil.copytree
        comm_obj.run = local

        _push_release(options.releasedir, destdir, comm_obj,
                      py=options.py)
    else: # assume destdir is a remote user@host:destdir

        # the only remote push destination should be the production server
        # at openmdao.org, which doesn't have python 2.7 as default, so...
        if options.py == 'python':
            options.py = 'python2.7'

        comm_obj.put = put
        comm_obj.put_dir = put_dir
        comm_obj.run = run

        destparts = destdir.split(':', 1)
        if len(destparts) > 1:
            home = destparts[1]
        else:
            home = '~'

        with settings(host_string=destparts[0]):
            _push_release(options.releasedir, home, comm_obj,
                          py=options.py)
