
"""
A script to run an OpenMDAO branch test triggered by a post_recieve
hook on github.
"""

import os
import sys
import shutil
import json
import pprint
import StringIO
import subprocess
import time
from threading import Thread
from Queue import Queue

import web

REPO_URL = 'https://github.com/OpenMDAO/OpenMDAO-Framework'
REPO_DIR = '/home/openmdao/install/OpenMDAO-Framework'
RESULTS_DIR = os.path.join(REPO_DIR,'devenv','host_results')
RESULTS_EMAILS = ['naylor.b@gmail.com']
PY = 'python2.6'
HOSTS = ['meerkat32_instance']
TEST_ARGS = ['--', '-v', 'openmdao.util.test.test_namelist']


def activate_and_run(envdir, cmd):
    """"
    Runs the given command from within an activated OpenMDAO virtual environment located
    in the specified directory.
    
    Returns the output and return code of the command as a tuple (output, returncode).
    """
    if sys.platform.startswith('win'):
        devbindir = 'Scripts'
        command = ['activate.bat',  '&&'] + cmd
    else:
        devbindir = 'bin'
        command = ['. ./activate', '&&'] + cmd
    
    # activate the environment and run command
    devbinpath = os.path.join(envdir, devbindir)
    os.chdir(devbinpath)
    print("running %s from %s" % (command, devbinpath))
    env = os.environ.copy()
    for name in ['VIRTUAL_ENV','_OLD_VIRTUAL_PATH','_OLD_VIRTUAL_PROMPT']:
        if name in env: 
            del env[name]

    return _run_sub(' '.join(command), env=env, shell=True)

def _run_sub(cmd, **kwargs):
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT, **kwargs)
    output = p.communicate()[0]
    return (output, p.returncode)

def do_tests(q):
    while True:
        payload = q.get(block=True)
        test_commit(payload)
        
def send_mail(commit_id, retval, msg):
    status = 'succeeded' if retval == 0 else 'failed'
    web.sendmail('openmdao@web103.webfaction.com',
                 RESULTS_EMAILS,
                 'test %s for commit %s' % (status, commit_id),
                 msg)

def test_commit(payload):
        pprint.pprint(payload)
        print '\n\n--------------------------\n\n'
        
        repo = payload['repository']['url']
        commit_id = payload['after']
        branch = payload['ref'].split('/')[-1]
        
        if repo != REPO_URL:
            print 'repo URL %s does not match expected repo URL (%s)' % (repo, REPO_URL)
            return
        
        if branch != 'dev':
            print 'branch is %s' % branch
            print 'ignoring commit %s: not on dev branch' % commit_id
            return
        
        out, ret = _run_sub('git checkout dev', shell=True)
        print out
        if ret != 0:
            send_mail(commit_id, ret, out)
            return
        
        out, ret = _run_sub('git pull origin dev', shell=True)
        print out
        if ret != 0:
            send_mail(commit_id, ret, out)
            return
    
        tmp_results_dir = os.path.join(RESULTS_DIR, commit_id)
        
        cmd = ['test_branch', 
               '-o', tmp_results_dir,
               #'-f', repo,
               #'--branch=%s' % branch,
               ]
        for host in HOSTS:
            cmd.append('--host=%s' % host)
            
        cmd += TEST_ARGS
        
        os.mkdir(tmp_results_dir)
        try:
            out, ret = activate_and_run(os.path.join(REPO_DIR,'devenv'),
                                        cmd)
            if ret != 0:
                print out
                send_mail(commit_id, ret, out)
                return
        finally:
            shutil.rmtree(tmp_results_dir)

class TestRunner:
    def __init__(self, q):
        self.q = q

    #def GET(self):
        #i = web.input(name = 'web')
        #return 'Hello, ' + web.websafe(i.name) + '!'

    def POST(self):
        data = web.input('payload')
        payload = json.loads(data.payload)
        self.q.put(payload)
        
    def __call__(self):
        return self

if __name__ == "__main__":
    os.chdir(os.path.join(REPO_DIR,'devenv'))
    
    urls = ('/', 'runtests')
    q = Queue()
    runtests = TestRunner(q)
    tester = Thread(target=do_tests, name='tester', args=(q,))
    tester.daemon = True
    tester.start()
    app = web.application(urls, {'runtests': runtests})
    app.run()


