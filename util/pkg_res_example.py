from pkg_resources import get_entry_map
from pkg_resources import Environment


import sys
import rfc822
import StringIO

env = Environment([sys.argv[1]])
dists = env[sys.argv[2]]

for dist in dists:
    print 'project_name = ',dist.project_name
    print 'version = ',dist.version
    print 'py_version = ',dist.py_version
    print 'platform = ',dist.platform
    print 'entry points are:'
    for gname,group in get_entry_map(dist, group=None).items():
        print '['+gname+']'
        for ep in group:
            print ep

    meta = dist.get_metadata('PKG-INFO')
    instr = StringIO.StringIO(meta)
    message = rfc822.Message(instr)
    
    for k,v in message.items():
        print k+':',v

