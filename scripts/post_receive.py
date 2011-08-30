
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

import web

REPO_URL = 'https://github.com/OpenMDAO/OpenMDAO-Framework'
REPO_DIR = '/home/openmdao/install/OpenMDAO-Framework'
RESULTS_DIR = os.path.join(REPO_DIR,'devenv','host_results')
RESULTS_EMAILS = ['naylor.b@gmail.com']
PY = 'python2.6'


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

class runtests:
    #def GET(self):
        #i = web.input(name = 'web')
        #return 'Hello, ' + web.websafe(i.name) + '!'

    def POST(self):
        data = web.input('payload')
        payload = json.loads(data.payload)
        pprint.pprint(payload)
        
        repo = payload['repository']['url']
        if repo != REPO_URL:
            print 'repo URL %s does not match expected repo URL (%s)' % (repo, REPO_URL)
        
        commit_id = payload['after']
        branch = payload['ref'].split('/')[-1]
        
        tmp_results_dir = os.path.join(RESULTS_DIR, commit_id)
        os.mkdir(tmp_results_dir)
        
        cmd = ['test_branch', 
               '-o', tmp_results_dir,
               '-f', repo,
               '--branch=%s' % branch,
               ]
        
        try:
            output, retval = activate_and_run(os.path.join(REPO_DIR,'devenv'),
                                              cmd)
            
            if retval == 0:
                status = 'succeeded'
            else:
                status = 'failed'
                
            web.sendmail('openmdao@web103.webfaction.com',
                         RESULTS_EMAILS,
                         'test %s for https://github.com/OpenMDAO/OpenMDAO-Framework/commit/%s' % (status,commit_id),
                         output)
        finally:
            shutil.rmtree(tmp_results_dir)
            

if __name__ == "__main__":
    urls = ('/', 'runtests')
    app = web.application(urls, globals())
    app.run()


