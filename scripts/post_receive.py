
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

#REPO_URL = 'https://github.com/OpenMDAO/OpenMDAO-Framework'
REPO_URL = 'https://github.com/naylor-b/OpenMDAO-Framework'
REPO_DIR = '/home/openmdao/install/OpenMDAO-Framework'
REPO_BRANCHES = ['foo']
REMOTE_NAME = 'bret'
RESULTS_DIR = os.path.join(REPO_DIR,'devenv','host_results')
RESULTS_EMAILS = ['naylor.b@gmail.com']
PY = 'python2.6'
HOSTS = ['meerkat32_instance']
TEST_ARGS = ['--', '-v', 'openmdao.util.test.test_namelist']


def _has_checkouts():
    cmd = 'git status -s'
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, 
                         env=os.environ, shell=True)
    out = p.communicate()[0]
    ret = p.returncode
    if ret != 0:
        raise RuntimeError(
             'error while getting status of git repository from directory %s (return code=%d): %s'
              % (os.getcwd(), ret, out))
    for line in out.split('\n'):
        line = line.strip()
        if len(line)>1 and not line.startswith('?'):
            return True
    return False

def activate_and_run(envdir, cmd):
    """"
    Runs the given command from within an activated OpenMDAO virtual environment located
    in the specified directory.
    
    Returns the output and return code of the command as a tuple (output, returncode).
    """
    if sys.platform.startswith('win'):
        command = ['Scripts/activate.bat',  '&&'] + cmd
    else:
        command = ['source ./bin/activate', '&&'] + cmd
    
    # activate the environment and run command
    os.chdir(envdir)
    print("running %s from %s" % (' '.join(command), envdir))
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
        print 'got a commit!!!!'
        try:
            test_commit(payload)
        except Exception as err:
            print str(err)
        
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
            print 'ignoring commit: repo URL %s does not match expected repo URL (%s)' % (repo, REPO_URL)
            return
        
        if branch not in REPO_BRANCHES:
            print 'branch is %s' % branch
            print 'ignoring commit %s: branch is not one of %s' % (commit_id,
                                                                   REPO_BRANCHES)
            return
        
        if _has_checkouts():
            send_mail(commit_id, -1, 'branch %s is not clean!' % branch)
            return
        
        out, ret = _run_sub('git checkout %s' % branch, shell=True)
        print out
        if ret != 0:
            send_mail(commit_id, ret, out)
            return
        
        out, ret = _run_sub('git pull %s %s' % (REMOTE_NAME,
                                                branch), shell=True)
        print out
        if ret != 0:
            send_mail(commit_id, ret, out)
            return
    
        tmp_results_dir = os.path.join(RESULTS_DIR, commit_id)
        
        cmd = ['test_branch', 
               '-o', tmp_results_dir,
               ]
        for host in HOSTS:
            cmd.append('--host=%s' % host)
            
        cmd += TEST_ARGS
        
        os.makedirs(tmp_results_dir)
        try:
            out, ret = activate_and_run(os.path.join(REPO_DIR,'devenv'), cmd)
            print out
            if ret == 0:
                send_mail(commit_id, ret, out)
                shutil.rmtree(tmp_results_dir)
            else:
                send_mail(commit_id, ret, collect_results(tmp_results_dir))
        except Exception as err:
            send_mail(commit_id, -1, str(err))


def collect_results(tmp_results_dir):
    print 'collecting results from %s' % tmp_results_dir
    results = StringIO.StringIO()
    for d in os.listdir(tmp_results_dir):
        with open(os.path.join(tmp_results_dir, d, 'run.out'), 'r') as f:
            s = f.read()
            results.write(s)
            results.write('\n---------------------------------\n')
    return results.getvalue()


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
        
    # this is just a kludge to let me use an instance instead of a class
    # with web.py
    def __call__(self):
        return self

if __name__ == "__main__":
    os.chdir(os.path.join(REPO_DIR,'devenv'))
    
    urls = ('/', 'runtests')
    q = Queue()
    runtests = TestRunner(q)
    tester = Thread(target=do_tests, name='tester', args=(q,))
    tester.daemon = True
    print 'starting tester thread'
    tester.start()
    app = web.application(urls, {'runtests': runtests})
    print 'running app'
    app.run()


