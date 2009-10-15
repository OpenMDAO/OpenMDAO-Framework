import os
import re
import subprocess
import tempfile
from subprocess import Popen,PIPE,STDOUT


ticket_rgx = re.compile('T[0-9]+')  
version_rgx = re.compile('[0-9]+(.[0-9]+)*')
release_rgx = re.compile('REL[0-9]+(.[0-9]+)*')

logtemplate = """\
revno: %(revno)s
committer: %(committer)s
branch_nick: %(branch_nick)s
timestamp: %(timestamp)s
tags: %(tags)s
message: %(message)s
"""

class LogInfo(object):
    def __init__(self):
        self.revno = -1
        self.committer = ''
        self.branch_nick = ''
        self.timestamp = ''
        self.message = ''
        self.tags = []
        self.position = 0
    
    def __repr__(self):
        return logtemplate % self.__dict__
        
    def __str__(self):
        return self.__repr__()
        

def _get_line(tf):
    line = tf.next().strip()
    stop = False
    if line.startswith('--------------------------------'):
        name = None
        val = None
    elif line.startswith('message:'):
        msglines = []
        line = tf.next().strip()
        while not line.startswith('--------------------------------'):
            msglines.append(line)
            try:
                line = tf.next().strip()
            except StopIteration:
                stop = True
                break
        name = 'message'
        val = '\n'.join(msglines)
    else:
        name, val = line.split(':', 1)
        val = val.strip()
        if name == 'tags':
            val = [x.strip() for x in val.split(',') if x.strip()]
    return (name, val, stop)

    
def get_loginfos(revs=None):
    loginfs = []
       
    try:
        if revs is not None:
            revstr = '-r'+revs[0]+'..'+revs[1]
        else:
            revstr = ''
        tf, tfname = tempfile.mkstemp()
        p = Popen('bzr log --forward '+revstr, 
                  stdout=tf, stderr=STDOUT, env=os.environ, shell=True)
        p.wait()
        os.close(tf)

        tf = open(tfname, 'r')

        try:
            line = tf.next()   # initial dashed line
            while(True):
                loginf = LogInfo()
                loginfs.append(loginf)
                loginf.position = len(loginfs)-1
                while(True):
                    name, val, stop = _get_line(tf)
                    if name is None:  # it's a separator line
                        break
                    setattr(loginf, name.replace(' ','_'), val)
                    if stop or name == 'message':
                        break
                if stop:
                    break
                
        except StopIteration:
            pass
        finally:
            tf.close()
    finally:
        os.remove(tfname)
        
    return loginfs
    
    
def get_branch_dict(loginfos):
    dct = {}
    for loginfo in loginfos:
        if loginfo.branch_nick:
            if loginfo.branch_nick not in dct:
                dct[loginfo.branch_nick] = []
            dct[loginfo.branch_nick].append(loginfo)
    return dct
            
def get_tag_dict(loginfos):
    dct = {}
    for loginfo in loginfos:
        for tag in loginfo.tags:
            dct[tag] = loginfo
    return dct
    
def get_rev_dict(loginfos):
    dct = {}
    for loginfo in loginfos:
        dct[loginfo.revno] = loginfo
    return dct
    
            
            
if __name__ == '__main__':
    loginfos = get_loginfos()
    tagdict = get_tag_dict(loginfos)
    branchdict = get_branch_dict(loginfos)
    print 'TinaTestTag =>\n ',tagdict['TinaTestTag']

