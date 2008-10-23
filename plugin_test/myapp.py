import os
import sys
from pkg_resources import working_set,WorkingSet,Environment,Requirement,Distribution
from pkg_resources import VersionConflict,DistributionNotFound,get_entry_map,load_entry_point

pkg_env = Environment(['plugins'])

distribs = []


def test_plugins():
    for name in pkg_env:
        # Environment[name] gives us a list of distribs for that project name
        for dist in pkg_env[name]:
            print 'distribution:',name
            print '  version: ',dist.version
            print '  location: ',dist.location
            print '  entry points: ',get_entry_map(dist, group='openmdao.components')
            distribs.append(dist)

        dist = distribs[0]
        
        print 'foo' in sys.modules
        
        dist.activate()
        foo_entry = load_entry_point(dist, group='openmdao.components', name='Comp1')
        print foo_entry
        ff = foo_entry()
        ff.do_something()
        
        print 'foo' in sys.modules
        


def get_available_plugins(search_path, groupname):
    """Given a search path and an entry point group name, return a dict of the
    form { compname1 : version,[entry_pt1, entry_pt2, ...], compname2 :
    version,[ep1, ep2, ...],...} This only searches distributions and gathers
    information. It does not activate any  distributions.
    """
    plugins = {}
    pkg_env = Environment(search_path)
    for name in pkg_env:
        plugins[name] = {}
        # Environment[name] gives us a list of distribs for that project name
        plugins[name] = []
        for dist in pkg_env[name]:
            print dir(dist)
            entry_dict = get_entry_map(dist, group=groupname)
            plugins[name].append([dist.version, entry_dict.values()])
            
    return plugins


def get_dists_with_plugins(search_path, groupname):
    dists = {}
    pkg_env = Environment(search_path)
    for name in pkg_env:  # loop over packages
        dists[name] = []
        for dist in pkg_env[name]:  # loop over different dist versions of a package
            entry_dict = get_entry_map(dist, group=groupname)
            if len(entry_dict) > 0:
                dists[name].append(dist)
            
    return dists


def find_egg_in_modpath(mpath):
    """Return the pathname of the egg file if the module path contains one."""
    head = mpath
    while head != '':
        if head.endswith('.egg'):
            return head
        head, tail = os.path.split(head)
        

def inst(req):
    print "couldn't meet requirement",req
    
        
def import_version(req, env=None):
    """Import the project version specified in the Requirement req, if it can be 
    found in the current WorkingSet or in the specified Environment.
    If a conflicting version already exists in the WorkingSet, raise a 
    VersionConflict.  If a distrib cannot be found matching the requirement,
    raise a DistributionNotFound."""
    
    needed = working_set.resolve([req], env)

    for dist in needed:
        # add required distribs to the real working set 
        if dist is not None:
            working_set.add(dist,entry=None,insert=False)  
            dist.activate() 
            __import__(dist.project_name)
        

try:
    import_version(Requirement.parse('foobar==1.0'),
                         Environment(['plugins']))
except DistributionNotFound, err:
    print str(err)

import_version(Requirement.parse('bar==1.0'),Environment(['plugins']))
import_version(Requirement.parse('foo==1.4'),Environment(['plugins']))

            
"""
dists = get_dists_with_plugins(['plugins'], 'openmdao.components')
for project,distlist in dists.items():
    print 'project:',project
    for dist in distlist:
        print dist.version, dist.location 
        dist.activate()   
        emap = get_entry_map(dist, group='openmdao.components')
        for ename,entry in emap.items():
            print 'loading ',ename
            try:
                #comp = load_entry_point(dist, group='openmdao.components', name=ename)
                klass = entry.load()
                obj = klass()
                obj.do_something()
            except Exception, err:
                print 'Caught: ',str(err)
        print dist.check_version_conflict()
"""

    
