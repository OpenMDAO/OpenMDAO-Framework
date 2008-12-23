"""
A Factory that creates (using pkg_resources) plugins coming from distributions.
"""

#public symbols
#__all__ = []

__version__ = "0.1"

# these fail to find pkg_resources when run from pylint
# pylint: disable-msg=F0401
from pkg_resources import working_set, get_entry_map
from pkg_resources import Environment, Requirement, DistributionNotFound
    
from openmdao.main.factory import Factory


def import_version(req, env=None):
    """Import the project version specified in the Requirement req, if it can
    be found in the current WorkingSet or in the specified Environment. If a
    conflicting version already exists in the WorkingSet, a VersionConflict
    will be raised. If a distrib cannot be found matching the requirement,
    raise a DistributionNotFound.
    """

    # see if the requested distrib is available either in the current 
    # working_set or in the environment
    try:
        needed = working_set.resolve([req], env)
    except DistributionNotFound:
        raise DistributionNotFound('could not find distribution satisfying '+
                                   str(req))

    for dist in needed:
        # add required distribs to the real working set
        if dist not in working_set:
            working_set.add(dist,entry=None,insert=False)  
            dist.activate()
                
    for dist in needed:
        __import__(dist.project_name)           


class EntryPtLoader(object):
    """Holder of entry points. Will perform lazy importing of 
    distributions as needed.
    """
    def __init__(self, name, group, dist, entry_pt):
        self.name = name
        self.group = group
        self.dist = dist
        self.entry_pt = entry_pt
        self.ctor = None
    
    def create(self, env, name):
        """Return the object created by calling the entry point specified by
        typ. If necessary, first activate the distribution and load the entry
        point, and check for conflicting version dependencies before loading.
        """
        if self.ctor is None:
            import_version(self.dist.as_requirement(), env)
            self.ctor = self.entry_pt.load(require=False, env=env)
            
        return self.ctor(name)

                
class PkgResourcesFactory(Factory):
    """A Factory that loads plugins using the pkg_resources API, which means
    it searches through egg info of distributions in order to find any entry
    point groups corresponding to openmdao plugin types, e.g.,
    openmdao.components, openmdao.variables, etc.
    """
    
    def __init__(self, search_path=None, groups=None):
        super(PkgResourcesFactory, self).__init__()
        self.env = Environment(search_path)
        self._loaders = {}
        for group in groups:
            self._get_plugin_info(self.env, group)
        
        
    def create(self, typ, name=None, version=None, server=None, 
               res_desc=None):
        """Create and return an object of the given type, with
        optional name, version, server id, and resource description.
        """
        if server is not None or res_desc is not None:
            return None
        
        if version is None:
            return self._loaders[typ][0].create(self.env, name)
        
        for entry in self._loaders[typ]:
            if entry.dist in Requirement.parse(entry.dist.project_name+
                                               '=='+version):
                return entry.create(self.env, name)

    
    def _get_plugin_info(self, pkg_env, groupname):
        """Given a search path and an entry point group name, fill the
        self._loaders dict with EntryPtLoader objects. This only searches
        distributions and gathers information. It does not activate any
        distributions.
        """
        for name in pkg_env:
            # pkg_env[name] gives us a list of distribs for that package name
            for dist in pkg_env[name]:
                entry_dict = get_entry_map(dist, group=groupname)
                for entry_pt in entry_dict.values():
                    if len(entry_pt.attrs) > 0:
                        ename = '.'.join([entry_pt.module_name]+
                                         list(entry_pt.attrs))
                    else:
                        raise NameError('entry point in setup.py file'+
                                 ' must specify an object within the module')
                    if ename not in self._loaders:
                        self._loaders[ename] = []
                    self._loaders[ename].append(EntryPtLoader(name=ename, 
                                                            group=groupname,
                                                            dist=dist, 
                                                            entry_pt=entry_pt))
                

    def get_loaders(self, group, active=True):
        """Return a list of EntryPointLoaders with group ids that 
        match the given group.
        """
        matches = []
        for loaders in self._loaders.values():
            for loader in loaders:
                if loader.group == group:
                    if loader.ctor is not None or active is False:
                        matches.append(loader)
                
        return matches
    