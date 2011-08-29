
"""
A script to run an OpenMDAO branch test triggered by a post_recieve
hook on github.
"""

import os
import shutil
from optparse import OptionParser


def run_testbranch(argv=None):
    if argv is None:
        argv = sys.argv[1:]
        
    parser = OptionParser(usage="%prog [OPTIONS] -- [options to openmdao_test]")
    parser.add_option("-d", "--dir", action='store', dest='workingdir', 
                      help="working directory")
    parser.add_option("--host", action='append', dest='hosts', metavar='HOST',
                      default=[],
                      help="Select host from config file to run on. "
                           "To run on multiple hosts, use multiple --host args")
    parser.add_option("--all", action="store_true", dest='allhosts',
                      help="If True, run on all hosts in config file.")
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="Don't delete the temporary build directory. "
                           "If testing on EC2 stop the instance instead of terminating it.")

    (options, args) = parser.parse_args(argv)
    
    if not options.hosts and not options.allhosts:
        parser.print_help()
        print "nothing to do - no hosts specified"
        sys.exit(0)
    
    startdir = os.getcwd()
    
    fname = os.path.abspath(os.path.expanduser(options.fname))
    
    if not fname.endswith('.git'):
        parser.print_help()
        print "\nfilename must end in '.git'"
        sys.exit(-1)
        
    funct_kwargs = { 'keep': options.keep,
                     'testargs': args,
                     'fname': fname,
                     'remotedir': get_tmp_user_dir(),
                     'branch': options.branch,
                     }
        
            
    return retcode

def activate_and_test(envdir, testargs=()):
    """"
    Runs the test suite on an OpenMDAO virtual environment located
    in the specified directory.
    
    Returns the return code of the process that runs the test suite.
    """
    if sys.platform.startswith('win'):
        devbindir = 'Scripts'
        command = 'activate.bat && openmdao_test %s' % ' '.join(testargs)
    else:
        devbindir = 'bin'
        command = '. ./activate && openmdao_test %s' % ' '.join(testargs)
        
    # activate the environment and run tests
    devbinpath = os.path.join(envdir, devbindir)
    os.chdir(devbinpath)
    print("running tests from %s" % devbinpath)
    env = os.environ.copy()
    for name in ['VIRTUAL_ENV','_OLD_VIRTUAL_PATH','_OLD_VIRTUAL_PROMPT']:
        if name in env: 
            del env[name]
    return _run_sub('test.out', command, env=env)


class runtests:
    #def GET(self):
        #i = web.input(name = 'web')
        #return 'Hello, ' + web.websafe(i.name) + '!'

    def POST(self):
        data = web.input('payload')
        payload = json.loads(data.payload)
        
        repo = payload['repository']['url']
        if repo != REPO_URL:
            print 'repo URL %s does not match expected repo URL (%s)' % (repo, REPO_URL)
        
        commit_id = payload['after']
        tmp_results_dir = os.path.join(RESULTS_DIR, commit_id)
        os.mkdir(tmp_results_dir)
        
        try:
            f = StringIO.StringIO()
            f.write("repo url: %s\n" % payload['repository']['url'])
            f.write("before: %s\n" % payload['before'])
            f.write("after: %s\n" % payload['after'])
            f.write("ref: %s\n" % payload['ref'])
            f.write("commits:\n")
            for commit in payload['commits']:
                f.write("%s: %s\n" % (commit['author']['name'],
                                      commit['message']))
            print f.getvalue()
            web.sendmail('openmdao@web103.webfaction.com',
                         ['naylor.b@gmail.com'],
                         'your latest test results',
                         f.getvalue())
        finally:
            shutil.rmtree(tmp_results_dir)
            

if __name__ == "__main__":
    urls = ('/', 'runtests')
    app = web.application(urls, globals())
    app.run()


