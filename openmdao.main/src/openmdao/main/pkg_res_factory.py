
#public symbols
__all__ = ['import_version', 'EntryPtLoader', 'PkgResourcesFactory' ]

import logging
import copy
import os.path

# these fail to find pkg_resources when run from pylint
# pylint: disable-msg=F0401
import pkg_resources
from pkg_resources import get_entry_map, get_distribution
from pkg_resources import Environment, Requirement, DistributionNotFound
    
import openmdao.main.factory 
Factory = openmdao.main.factory.Factory

def import_version(modname, req, env=None):
    """Import the specified module from the package specified in the
    Requirement req, if it can be found in the current WorkingSet or in the
    specified Environment. If a conflicting version already exists in the
    WorkingSet, a VersionConflict will be raised. If a distrib cannot be found
    matching the requirement, raise a DistributionNotFound.
    """

    # see if the requested distrib is available either in the current 
    # working_set or in the environment
    try:
        needed = pkg_resources.working_set.resolve([req], env)
    except DistributionNotFound:
        raise DistributionNotFound('could not find distribution satisfying '+
                                   str(req))

    for dist in needed:
        # add required distribs to the real working set
        if dist not in pkg_resources.working_set:
            pkg_resources.working_set.add(dist)
            dist.activate()
                
    __import__(modname)


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
    
    def create(self, env, **ctor_args):
        """Return the object created by calling the entry point.If
        necessary, first activate the distribution and load the entry
        point, and check for conflicting version dependencies before
        loading. """
        if self.ctor is None:
            import_version(self.entry_pt.module_name,
                           self.dist.as_requirement(), env)
            self.ctor = self.entry_pt.load(require=False, env=env)
            
        logging.debug('calling %s(%s)' % (self.ctor.__name__, ctor_args))
        obj = self.ctor(**ctor_args)
        obj.parent = None
        return obj

                
class PkgResourcesFactory(Factory):
    """A Factory that loads plugins using the pkg_resources API, which means
    it searches through egg info of distributions in order to find any entry
    point groups corresponding to openmdao plugin types, e.g.,
    openmdao.component, openmdao.trait, etc.
    """
    
    def __init__(self, groups, search_path=None):
        super(PkgResourcesFactory, self).__init__()
        self._groups = copy.copy(groups)
        
        self.update_search_path(search_path)
    
    def update_search_path(self, search_path):
        self.env = Environment(search_path)
        self._loaders = {}
        for group in self._groups:
            self._get_plugin_info(self.env, group)
        
    def create(self, typ, version=None, server=None, 
               res_desc=None, **ctor_args):
        """Create and return an object of the given type, with
        optional name, version, server id, and resource description.
        """
        if server is not None or res_desc is not None:
            return None

        try:
            if version is None:
                # TODO: make sure this always creates the newest version
                return self._loaders[typ][0].create(self.env, **ctor_args)

            for entry in self._loaders[typ]:
                if entry.dist in Requirement.parse(entry.dist.project_name+
                                                   '=='+version):
                    return entry.create(self.env, **ctor_args)
        except KeyError:
            pass
        return None
            
    
    def _get_plugin_info(self, pkg_env, groupname):
        """Given a search path and an entry point group name, fill the
        self._loaders dict with EntryPtLoader objects. This only searches
        distributions and gathers information. It does not activate any
        distributions.
        """
        for name in pkg_env:
            # pkg_env[name] gives us a list of distribs for that package name
            for dist in pkg_env[name]:
                try:
                    entry_dict = get_entry_map(dist, group=groupname)
                except IOError:
                    continue  # Probably due to removal of egg (tests, etc.)

                for nm, entry_pt in entry_dict.items():
                    if len(entry_pt.attrs) > 0:
                        if nm not in self._loaders:
                            self._loaders[nm] = []
                        # avoid adding a loader we already have
                        for loader in self._loaders[nm]:
                            if dist == loader.dist:
                                break
                        else:
                            self._loaders[nm].append(
                                EntryPtLoader(name=nm, group=groupname,
                                              dist=dist, entry_pt=entry_pt))
                    else:
                        raise NameError('entry point '+nm+' in setup.py file'+
                                 ' must specify an object within the module')
                

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
    

    def get_available_types(self, groups=None):
        """Return a set of tuples of the form (typename, dist_version), one
        for each available plugin type in the given entry point groups.
        If groups is None, return the set for all openmdao entry point groups.
        """
        ret = []
        for group in groups:
            ret.extend([(l.name, l.dist.version) for l in self.get_loaders(group, False)])
        return ret

