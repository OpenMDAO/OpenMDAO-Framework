
import os
import os.path
import sys
import tarfile

from fabric.api import run, env, local, put, cd, get, settings
from fabric.state import connections

from openmdao.devtools.build_docs import build_docs, _get_dirnames


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


if __name__ == "__main__": #pragma: no cover
    push_docs()


