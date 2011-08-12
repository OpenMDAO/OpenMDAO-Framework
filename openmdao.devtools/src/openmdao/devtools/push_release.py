
import sys
import os
import shutil
import urllib2
import fnmatch
import tempfile
import tarfile
import atexit
from optparse import OptionParser

from fabric.api import run, env, local, put, cd, prompt, hide, hosts, get, settings
from fabric.state import connections

from openmdao.devtools.utils import get_openmdao_version, put_dir, tar_dir, \
                                    repo_top, fabric_cleanup

#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')

def _push_release(release_dir, destination, obj):
    """Take a directory containing release files (openmdao package distributions,
    install scripts, etc., and place the files in the proper locations on the
    server.
    
    release_dir: str
        where the release file are located
        
    destination: str
        the location where the release files are to be placed. It can be a URL
        or a local directory
        
    obj: _CommObj
        an object to wrap the behaviors of run, put, etc. so calls are the 
        same for local or remote release areas
    """
    files = os.listdir(release_dir)
    f = fnmatch.filter(files, 'go-openmdao-*.py')
    if len(f) < 1:
        raise RuntimeError("can't find go-openmdao-*.py file in release directory")
    elif len(f) > 1:
        raise RuntimeError("more than one file in release dir matches 'go-openmdao-*.py'")
    script = f[0]
    
    # determine version from form of the go-openmdao-?.?.py file
    version = os.path.splitext(script)[0].split('-', 2)[2]
    
    # the following will barf if the version already exists on the server
    obj.run('mkdir %s/downloads/%s' % (destination, version))
    obj.run('chmod 755 %s/downloads/%s' % (destination, version))

    # push new distribs to the server
    for f in os.listdir(release_dir):
        if (f.endswith('.tar.gz') and f != 'docs.tar.gz') or f.endswith('.egg'):
            obj.put(os.path.join(release_dir,f), '%s/dists/%s' % (destination, f))
            obj.run('chmod 644 %s/dists/%s' % (destination, f))
    
    # for now, put the go-openmdao script up without the version
    # id in the name
    obj.put(os.path.join(release_dir, 'go-openmdao-%s.py' % version), 
        '%s/downloads/%s/go-openmdao.py' % (destination, version))
    obj.run('chmod 755 %s/downloads/%s/go-openmdao.py' % (destination, version))

    # put the docs on the server
    obj.put_dir(os.path.join(release_dir, 'docs'), 
                '%s/downloads/%s/docs' % (destination, version))

    obj.put(os.path.join(repo_top(),'scripts','mkdlversionindex.py'), 
            '%s/downloads/%s/mkdlversionindex.py' % (destination, version))
    
    obj.put(os.path.join(repo_top(),'scripts','mkegglistindex.py'), 
            '%s/dists/mkegglistindex.py' % destination)
    
    obj.put(os.path.join(repo_top(),'scripts','mkdownloadindex.py'), 
            '%s/downloads/mkdownloadindex.py' % destination)
    
    # update the index.html for the version download directory on the server
    with cd('%s/downloads/%s' % (destination, version)):
        obj.run('python2.6 mkdlversionindex.py')

    # update the index.html for the dists directory on the server
    with cd('%s/dists' % destination):
        obj.run('python2.6 mkegglistindex.py')

    # update the 'latest' link
    obj.run('rm -f %s/downloads/latest' % destination)
    obj.run('ln -s -f %s %s/downloads/latest' % (version, destination))
        
    # update the index.html for the downloads directory on the server
    with cd('%s/downloads' % destination):
        obj.run('python2.6 mkdownloadindex.py')
        


class _CommObj(object):
    pass

def main():
    parser = OptionParser(usage="%prog RELEASE_DIR DESTINATION")
    (options, args) = parser.parse_args(sys.argv[1:])
    
    comm_obj = _CommObj()
    
    if len(args) != 2:
        parser.print_help()
        sys.exit(-1)
        
    if not os.path.isdir(args[0]):
        print "release directory %s not found" % args[0]
        sys.exit(-1)
    
    destparts = args[1].split(':', 1)
    if len(destparts)==1 and os.path.isdir(args[1]):  # it's a local release test area
        comm_obj.put = shutil.copy
        comm_obj.put_dir = shutil.copytree
        comm_obj.run = local
        
        _push_release(args[0], args[1], comm_obj)
    else: # assume args[1] is a remote host:destdir
        comm_obj.put = put
        comm_obj.put_dir = put_dir
        comm_obj.run = run
        
        if len(destparts) > 1:
            home = destparts[1]
        else:
            home = '~'

        with settings(host_string=destparts[0]):
            _push_release(args[0], home, comm_obj)

if __name__ == '__main__':
    atexit.register(fabric_cleanup, True)
    main()
    
    