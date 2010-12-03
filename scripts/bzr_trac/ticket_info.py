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
        self.children = []
        self.parent = None
        self.indent = 0
    
    def __repr__(self):
        return logtemplate % self.__dict__
        
    def __str__(self):
        return self.__repr__()
        

def _get_line(tf):
    line = tf.next()
    stripline = line.strip()
    stop = False
    spaces = None
    if '--------------------------------' in line:
        name = None
        val = None
        spaces = line.find('-')
    elif stripline.startswith('message:'):
        msglines = []
        line = tf.next()
        while '--------------------------------' not in line:
            msglines.append(line.strip())
            try:
                line = tf.next()
            except StopIteration:
                stop = True
                break
        else:
            spaces = line.find('-')
        name = 'message'
        val = '\n'.join(msglines)
    else:
        name, val = stripline.split(':', 1)
        val = val.strip()
        if name == 'tags':
            val = [x.strip() for x in val.split(',') if x.strip()]
    return (name, val, spaces, stop)

    
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
            while(True):
                while(True):
                    name, val, spaces, stop = _get_line(tf)
                    if spaces is not None:  # it's a separator line
                        loginf = LogInfo()
                        loginf.indent = spaces
                        loginfs.append(loginf)
                        if name is None:
                            break
                    setattr(loginf, name.replace(' ','_'), val)
                    if stop:
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

def get_loginfo_tree(loginfos):
    tree = []
    for i,loginf in enumerate(loginfos):
        if loginf.indent == 0:
            tree.append(loginf)
            continue
        idx = i-1
        while idx >= 0:
            if loginf.indent > loginfos[idx].indent:
                loginfos[idx].children.append(loginf)
                loginf.parent = loginfos[idx]
                break
            idx -= 1
    return tree

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
    

def _walk_tree(tree):
    for loginf in tree:
        yield loginf
        for obj in loginf.children:
            yield obj
            
            
def walk_tree_chrono(tree):
    for loginf in tree:
        for obj in walk_tree_chrono(loginf.children):
            yield obj
        yield loginf


if __name__ == '__main__':
    loginfos = get_loginfos()
    tagdict = get_tag_dict(loginfos)
    branchdict = get_branch_dict(loginfos)
    print 'TinaTestTag =>\n ',tagdict['TinaTestTag']
    tree = get_loginfo_tree(loginfos)
    for obj in _walk_tree(tree):
        print ' '*obj.indent,obj.revno
    print 'tree[0].revno = ',tree[0].revno
    print 'tree[%s].revno = %s' % (len(tree)-1, tree[len(tree)-1].revno)
    