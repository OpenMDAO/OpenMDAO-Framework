
import os
from os.path import dirname, join, exists
import sys
import tarfile
import atexit
from optparse import OptionParser

from openmdao.devtools.build_docs import build_docs
from openmdao.devtools.utils import tar_dir, fabric_cleanup
from openmdao.main.plugin import print_sub_help


def push_docs(parser, options, args=None):
    """A script (push_docs) points to this. By default it pushes the current
    copy of the docs up to the development doc area on openmdao.org.
    """
    if args:
        print_sub_help(parser, 'push_docs')
        return -1
    
    from fabric.api import run, put, cd, settings
    atexit.register(fabric_cleanup, True)

    host = options.host

    startdir = os.getcwd()
    branchdir = dirname(dirname(dirname(sys.executable)))
    docdir = join(branchdir, 'docs')
    idxpath = join(docdir, '_build', 'html', 'index.html')
    
    if not os.path.isfile(idxpath) or not options.nodocbuild:
        build_docs()

    os.chdir(join(docdir, '_build'))
    tarpath = tar_dir('html', 'docs', '.')
    tarname = os.path.basename(tarpath)
    
    with settings(host_string=host):
        # tar up the docs so we can upload them to the server
        # put the docs on the server and untar them
        put(tarpath, '%s/%s' % (options.docdir, tarname))
        with cd(options.docdir):
            run('tar xzf %s' % tarname)
            run('rm -rf dev_docs')
            run('mv html dev_docs')
            run('rm -f %s' % tarname)



