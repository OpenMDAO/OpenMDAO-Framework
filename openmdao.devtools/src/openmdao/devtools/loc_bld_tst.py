import sys
import os
import shutil
import subprocess
import codecs
import subprocess
import fnmatch
import getpass
import datetime

def _run_sub(outname, cmd):
    # in some cases there are some unicode characters in the
    # build output which cause fabric to barf, so strip out unicode
    # by writing to a file, replacing unicode chars with '?'
    f = codecs.open(outname, 'wb', encoding='ascii', errors='replace')
    
    try:
        p = subprocess.Popen(cmd, stdout=f, stderr=subprocess.STDOUT,
                             shell=True)
        p.wait()
    finally:
        f.close()
        with open(outname, 'r') as f:
            print f.read()
    return p.returncode
        
def build_and_test(fname=None, workdir=None, pyversion='python', keep=False, 
                   branch=None, testargs=()):
    """Builds OpenMDAO, either a dev build or a release build, and runs
    the test suite on it.
    """
    if fname is None:
        raise RuntimeError("build_and_test: missing arg 'fname'")

    fname = os.path.abspath(fname)
    
    locbldfile = os.path.join(os.path.dirname(os.path.abspath(__file__)), 
                              'locbuild.py')
    loctstfile = os.path.join(os.path.dirname(os.path.abspath(__file__)), 
                              'loctst.py')
    if workdir is None:
        workdir = '%s_%s' % (getpass.getuser(),datetime.datetime.now())
        workdir = workdir.replace(' ','_').replace(':','.')
    
    workdir = os.path.abspath(workdir)
    
    if os.path.exists(workdir):
        cleanup = False
    else:
        os.mkdir(workdir)
        cleanup = True
    
    startdir = os.getcwd()
    
    if fname.endswith('.py'):
        build_type = 'release'
    else:
        build_type = 'dev'
        
    args = ['-f', fname]
        
    args.append('--pyversion=%s' % pyversion)
    if branch:
        args.append('--branch=%s' % branch)
        
    expectedfiles = set(['build.out', os.path.basename(fname)])
    
    os.chdir(workdir)
    dirfiles = set(os.listdir('.'))
    
    print 'building...'
    
    cmd = '%s %s %s' % (pyversion, locbldfile, ' '.join(args))
    try:
        retcode = _run_sub('build.out', cmd)
    finally:
        os.chdir(startdir)

    print "build return code =", retcode
    if retcode != 0:
        sys.exit(retcode)
    
    print '\ntesting...'
    os.chdir(workdir)
    
    newfiles = set(os.listdir('.')) - dirfiles - expectedfiles
    print 'newfiles = ',newfiles
    if build_type == 'dev':
        if len(newfiles) != 1:
            print "expected a single new dir in %s after building but got %s" % (remotedir, list(newfiles))
            sys.exit(-1)
        builddir = newfiles.pop()
        envdir = os.path.join(builddir, 'devenv')
    else: # test a release
        matches = fnmatch.filter(newfiles, 'openmdao-?.*')
        if len(matches) > 1:
            print "can't uniquely determine openmdao env directory from %s" % matches
            sys.exit(-1)
        elif len(matches) == 0:
            print "can't find an openmdao environment directory to test in"
            sys.exit(-1)
        envdir = matches[0]

    args = ['-d', envdir]
    args.append('--pyversion=%s' % pyversion)
    if keep:
        args.append('--keep')
    if len(testargs) > 0:
        args.append('--')
        args.extend(testargs)
        
    os.chdir(workdir)
    cmd = '%s %s %s' % (pyversion, loctstfile, ' '.join(args))
    try:
        retcode = _run_sub('test.out', cmd)
        
        if retcode == 0 and cleanup:
            shutil.rmtree(workdir)
    finally:
        os.chdir(startdir)

    return retcode

if __name__ == '__main__':
    from optparse import OptionParser
    
    parser = OptionParser(usage="%prog [OPTIONS]")
    parser.add_option("--pyversion", action="store", type='string', 
                      dest='pyversion', default="python", 
                      help="python version to use, e.g., 'python2.6'")
    parser.add_option("--branch", action="store", type='string', 
                      dest='branch',
                      help="if file_url is a git repo, supply branch name here")
    parser.add_option("-f","--file", action="store", type='string', 
                      dest='fname',
                      help="pathname or URL of a git repo, tar file, or go-openmdao.py file")
    parser.add_option("-d","--dir", action="store", type='string', 
                      dest='directory',
                      help="name of a directory the build will be created")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    sys.exit(build_and_test(fname=options.fname, workdir=options.directory,
                            pyversion=options.pyversion, 
                            branch=options.branch, testargs=args))
    
