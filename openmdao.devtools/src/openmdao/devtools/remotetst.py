import sys
import os
import subprocess
import atexit
import fnmatch
import tempfile

import paramiko.util
from argparse import ArgumentParser

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    push_and_run, rm_remote_tree, make_git_archive,\
                                    fabric_cleanup, remote_listdir, remote_mkdir,\
                                    ssh_test, put_dir, cleanup, remote_py_cmd, get
from openmdao.devtools.remote_cfg import add_config_options, process_options, \
                                         run_host_processes, get_tmp_user_dir, \
                                         print_host_codes

from openmdao.devtools.ec2 import run_on_ec2


def _remote_build_and_test(fname=None, pyversion='python', keep=False, 
                           branch=None, testargs='', hostname='', cfg=None,
                           **kwargs):
    if fname is None:
        raise RuntimeError("_remote_build_and_test: missing arg 'fname'")
    
    remotedir = get_tmp_user_dir()
    remote_mkdir(remotedir)
    
    locbldtstfile = os.path.join(os.path.dirname(__file__), 'loc_bld_tst.py')
    
    pushfiles = [locbldtstfile]
    
    build_type = 'release' if fname.endswith('.py') else 'dev'
        
    if cfg and cfg.has_option(hostname, 'pull_docs'):
        pull_docs = cfg.getboolean(hostname, 'pull_docs')
    else:
        pull_docs = False
        
    if os.path.isfile(fname):
        pushfiles.append(fname)
        remoteargs = ['-f', os.path.basename(fname)]
    elif os.path.isdir(fname):
        put_dir(fname, os.path.join(remotedir, os.path.basename(fname)))
        if sys.platform.startswith('win'):
            vername = 'latest' #readlink doesn't work on windows, so try 'latest'
        else:
            vername = os.readlink(os.path.join(fname,
                                               'downloads',
                                               'latest'))
        remoteargs = ['-f', os.path.join(os.path.basename(fname),
                                         'downloads', 
                                         vername,
                                         'go-openmdao.py')]
    else:
        remoteargs = ['-f', fname]
        
    if branch:
        remoteargs.append('--branch=%s' % branch)
        
    if testargs:
        remoteargs.append('--testargs="%s"' % testargs)
        
    try:
        result = push_and_run(pushfiles, runner=pyversion,
                              remotedir=remotedir, 
                              args=remoteargs)
        if pull_docs:
            if result.return_code == 0:
                print "pulling docs from %s" % hostname
                retrieve_docs(remotedir)
            else:
                print "not pulling docs from %s because test failed" % hostname
        else:
            print "not pulling docs from %s" % hostname
            
        return result.return_code
    finally:
        if not keep:
            print "removing remote directory: %s" % remotedir
            rm_remote_tree(remotedir)

def retrieve_docs(remote_dir):
    cmds = [ "import tarfile",
             "import os",
             "for fname in os.listdir('.'):",
             "    if fname.startswith('OpenMDAO-OpenMDAO-Framework') and not fname.endswith('.gz'):",
             "        break",
             "tardir = os.path.join(fname, 'docs', '_build', 'html')",
             "tar = tarfile.open('html.tar.gz', mode='w:gz')",
             "tar.add(tardir, arcname='html')",
             "tar.close()",
             ]
    result = remote_py_cmd(cmds, remote_dir=remote_dir)
    get('html.tar.gz')
    

def test_branch(options, args=None):
    atexit.register(fabric_cleanup, True)
    paramiko.util.log_to_file('paramiko.log')
    
    options.filters = ['test_branch==true']
    config, conn, ec2_hosts = process_options(options)
    
    if not options.hosts:
        parser.print_help()
        print "nothing to do - no hosts specified"
        sys.exit(0)
    
    startdir = os.getcwd()
    
    if options.fname is None: # assume we're testing the current repo
        print 'creating tar file of current branch: ',
        options.fname = os.path.join(os.getcwd(), 'testbranch.tar')
        ziptarname = options.fname+'.gz'
        cleanup(ziptarname) # clean up the old tar file
        make_git_archive(options.fname)
        subprocess.check_call(['gzip', options.fname])
        options.fname = os.path.abspath(ziptarname)
        print options.fname
        cleanup_tar = True
    else:
        cleanup_tar = False
        
    fname = options.fname
    if not (fname.startswith('http') or fname.startswith('git:') or fname.startswith('git@')):
        fname = os.path.abspath(os.path.expanduser(options.fname))
    
    if fname.endswith('.tar.gz') or fname.endswith('.tar'):
        if not os.path.isfile(fname):
            print "can't find file '%s'" % fname
            sys.exit(-1)
    elif fname.endswith('.git') or (fname.startswith('http') and os.path.splitext(fname)[1]==''):
        pass
    else:
        parser.print_help()
        print "\nfilename '%s' must specify a tar file or git repository" % fname
        sys.exit(-1)
        
    funct_kwargs = { 'keep': options.keep,
                     'testargs': options.testargs,
                     'fname': fname,
                     'remotedir': get_tmp_user_dir(),
                     'branch': options.branch,
                     'cfg': config
                     }
    try:
        retcode = run_host_processes(config, conn, ec2_hosts, options, 
                                     funct=_remote_build_and_test, 
                                     funct_kwargs=funct_kwargs,
                                     done_functs=[print_host_codes])
    finally:
        if cleanup_tar:
            cleanup(ziptarname)
    
    if retcode == 0:
        cleanup('paramiko.log')
        
    return retcode

def _is_release_dir(dname):
    if not isinstance(dname, basestring):
        return False
    if not os.path.isdir(dname):
        return False
    dirstuff = os.listdir(dname)
    if not 'dists' in dirstuff:
        return False
    return 'downloads' in dirstuff

def test_release(parser, options):
    atexit.register(fabric_cleanup, True)
    paramiko.util.log_to_file('paramiko.log')
    cleanup_files = [os.path.join(os.getcwd(), 'paramiko.log')]
    
    if options.fname is None:
        print '\nyou must supply a release directory or the pathname or URL of a go-openmdao.py file'
        sys.exit(-1)
        
    options.filters = ['test_release==true']
    config, conn, ec2_hosts = process_options(options)
    
    startdir = os.getcwd()
    
    fname = options.fname
    if fname.startswith('http'):
        # if they cut & paste from the openmdao website, the fname
        # will be followed by #md5=..., so get rid of that part
        fname = fname.split('#')[0]
    else:
        fname = os.path.abspath(os.path.expanduser(fname))
    
    if fname.endswith('.py'):
        if not fname.startswith('http'):
            if not os.path.isfile(fname):
                print "can't find file '%s'" % fname
                sys.exit(-1)
    elif _is_release_dir(fname):
        pass
    elif os.path.isdir(fname):
        #create a structured release directory
        release_dir = fname.replace('rel_', 'release_')
        subprocess.check_call(['release', 'push', fname, release_dir])
        fname = options.fname = release_dir
        cleanup_files.append(release_dir)
    else:
        parser.print_help()
        print "\nfilename must be a release directory or a pathname or URL of a go-openmdao.py file"
        sys.exit(-1)
        
    funct_kwargs = { 'keep': options.keep,
                     'testargs': options.testargs,
                     'fname': fname,
                   }
    retval = 0
    if len(options.hosts) > 0:
        retval = run_host_processes(config, conn, ec2_hosts, options, 
                                    funct=_remote_build_and_test, 
                                    funct_kwargs=funct_kwargs,
                                    done_functs=[print_host_codes])
    else: # just run test locally
        print 'testing locally...'
        loctst = os.path.join(os.path.dirname(__file__), 'loc_bld_tst.py')
        tdir = tempfile.mkdtemp()
        cleanup_files.append(tdir)
        if os.path.isdir(fname):
            if not _is_release_dir(fname):
                fname = release_dir
            fname = os.path.join(fname, 'downloads', 'latest', 'go-openmdao.py')
        cmd = [sys.executable, loctst, '-f', fname, '-d', tdir]
        if options.testargs:
            cmd.append('--testargs="%s"' % options.testargs)
        subprocess.check_call(cmd, stdout=sys.stdout, stderr=sys.stderr)
    
    if options.keep:
        print "the following files/directories were not cleaned up: %s" % cleanup_files
    else:
        cleanup(*cleanup_files)

        
# make nose ignore these functions
test_release.__test__ = False
test_branch.__test__ = False
_remote_build_and_test.__test__ = False
