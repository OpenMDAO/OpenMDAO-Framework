
from pkg_resources import working_set, get_entry_map, load_entry_point
from pkg_resources import Environment, Requirement, Distribution
from pkg_resources import VersionConflict, DistributionNotFound

#public symbols
__all__ = ['get_plugin_factories','import_version']

__version__ = "0.1"


from openmdao.main import Factory

def get_plugin_factories(search_path, groupname):
    """Given a search path and an entry point group name, return a list of 
    Factory objects. This only searches distributions and gathers information. 
    It does not activate any  distributions.
    """
    factories = []
    pkg_env = Environment(search_path)
    for name in pkg_env:
        # Environment[name] gives us a list of distribs for that project name
        for dist in pkg_env[name]:
            entry_dict = get_entry_map(dist, group=groupname)
            for entry_pt in entry_dict.values():
                factories.append(Factory(name=name, dist=dist, entry_pt=entry_pt))
            
    return factories

    
def import_version(req, env=None):
    """Import the project version specified in the Requirement req, if it can be 
    found in the current WorkingSet or in the specified Environment.
    If a conflicting version already exists in the WorkingSet, raise a 
    VersionConflict.  If a distrib cannot be found matching the requirement,
    raise a DistributionNotFound."""
    
    try:
        needed = working_set.resolve([req], env)
    except DistributionNotFound, err:
        raise DistributionNotFound('could not find distribution satisfying '+str(req))

    for dist in needed:
        # add required distribs to the real working set 
        if dist is not None:
            working_set.add(dist,entry=None,insert=False)  
            dist.activate() 
            __import__(dist.project_name)
        
            

if __name__ == '__main__':
    factories = get_plugin_factories(['/home/bnaylor/dev/OpenMDAO/plugin_test/plugins'], 'openmdao.components')
    for ff in factories:
        print ff.dist.location

