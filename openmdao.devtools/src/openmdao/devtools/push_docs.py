
import os
from os.path import dirname, join, exists
import sys
import tarfile

from fabric.api import run, env, local, put, cd, get, settings
from fabric.state import connections

from openmdao.devtools.build_docs import build_docs
from openmdao.devtools.utils import put_dir


def push_docs(argv=None):
    """A script (push_docs) points to this. By default it pushes the current
    copy of the docs up to the development doc area on openmdao.org.
    """
    if argv is None:
        argv = sys.argv[1:]

    if len(argv) < 1 or len(argv) > 2:
        print "usage: push_docs host [remote_dir]"
        sys.exit(-1)

    host = argv[0]
    if len(argv) > 1:
        remote_dir = argv[1]

    startdir = os.getcwd()
    branchdir = dirname(dirname(dirname(sys.executable)))
    docdir = join(branchdir, 'docs')
    idxpath = join(docdir, '_build', 'html', 'index.html')
    
    if not os.path.isfile(idxpath):
        build_docs()

    try:
        os.chdir(join(docdir, '_build'))
        try:
            if exists('docs.tar.gz'):
                os.remove('docs.tar.gz')
            archive = tarfile.open('docs.tar.gz', 'w:gz')
            archive.add('html')
            archive.close()
        finally:
            os.chdir(startdir)
        
        with settings(host_string=host):
            # tar up the docs so we can upload them to the server
            # put the docs on the server and untar them
            put(join(docdir,'_build','docs.tar.gz'), 'downloads/docs.tar.gz')
            with cd('downloads'):
                run('tar xzf docs.tar.gz')
                run('rm -rf dev_docs')
                run('mv html dev_docs')
                run('rm -f docs.tar.gz')
    finally:
        for key in connections.keys():
            connections[key].close()
            del connections[key]


if __name__ == "__main__": #pragma: no cover
    push_docs(sys.argv[1:])


