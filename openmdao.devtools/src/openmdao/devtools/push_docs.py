
import os
from os.path import dirname, join, exists
import sys
import tarfile
from optparse import OptionParser

from fabric.api import run, env, local, put, cd, get, settings
from fabric.state import connections

from openmdao.devtools.build_docs import build_docs
from openmdao.devtools.utils import tar_dir


def push_docs(argv=None):
    """A script (push_docs) points to this. By default it pushes the current
    copy of the docs up to the development doc area on openmdao.org.
    """
    if argv is None:
        argv = sys.argv[1:]

    parser = OptionParser(usage='%prog [OPTIONS] host')
    parser.add_option("-d", "--destination", action="store", type="string", 
                      dest="docdir", default='downloads',
                      help="directory where dev_docs directory will be placed")
    parser.add_option("-f", "--force", action="store_true", dest="force",
                      help="forces docs to be rebuilt")
    (options, args) = parser.parse_args(argv)
    
    if len(args) != 1:
        parser.print_help()
        sys.exit(-1)

    host = args[0]

    startdir = os.getcwd()
    branchdir = dirname(dirname(dirname(sys.executable)))
    docdir = join(branchdir, 'docs')
    idxpath = join(docdir, '_build', 'html', 'index.html')
    
    if options.force or not os.path.isfile(idxpath):
        build_docs()

    try:
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
    finally:
        for key in connections.keys():
            connections[key].close()
            del connections[key]


if __name__ == "__main__": #pragma: no cover
    push_docs(sys.argv[1:])


