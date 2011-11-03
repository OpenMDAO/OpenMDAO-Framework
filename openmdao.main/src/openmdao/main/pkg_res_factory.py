
import logging
import copy
import os.path

# these fail to find pkg_resources when run from pylint
# pylint: disable-msg=F0401
import pkg_resources
from pkg_resources import get_entry_map, get_distribution, working_set
from pkg_resources import Environment, WorkingSet, Requirement, DistributionNotFound
    
import openmdao.main.factory 
Factory = openmdao.main.factory.Factory



# this list should contain all openmdao entry point groups
_plugin_groups = [ 'openmdao.container',
                   'openmdao.component',
                   'openmdao.driver',
                   'openmdao.surrogatemodel',
                   'openmdao.differentiator',
                   'openmdao.variable',
                   'openmdao.architecture',
                   'openmdao.caseiterator',
                   'openmdao.caserecorder',
                   'openmdao.doegenerator',
                   'openmdao.optproblem',
                ]

                
class PkgResourcesFactory(Factory):
    """A Factory that loads plugins using the pkg_resources API, which means
    it searches through egg info of distributions in order to find any entry
    point groups corresponding to openmdao plugin types, e.g.,
    openmdao.component, openmdao.variable, etc.
    """
    
    def __init__(self, groups, search_path=None):
        super(PkgResourcesFactory, self).__init__()
        self._new_types = True
        self._groups = copy.copy(groups)
        self.env = Environment(search_path)
            
    def create(self, typ, version=None, server=None, 
               res_desc=None, **ctor_args):
        """Create and return an object of the given type, with
        optional name, version, server id, and resource description.
        """
        if server is not None or res_desc is not None:
            return None

        classes = self._get_type_dict()
            
        try:
            dist, group = classes[typ]
            klass = dist.load_entry_point(group, typ)
            
            if version is not None and dist.version != version:
                return None
            
            return klass(**ctor_args)
        except KeyError:
            # try to look in the whole environment
            for group in self._groups:
                for proj in self.env:
                    dists = self.env[proj]
                    for dist in dists:
                        if version is not None and version != dist.version:
                            continue
                        ep = dist.get_entry_info(group, typ)
                        if ep is not None:
                            dist.activate()
                            klass = ep.load(require=True, env=self.env)
                            self._new_types = True
                            return klass(**ctor_args)
                        if version is None:
                            # newest version didn't have entry point, so skip to next project
                            break
        return None
            
    def _get_type_dict(self):
        if self._new_types:
            dct = {}
            for group in _plugin_groups:
                for dist in working_set:
                    d = dist.get_entry_map(group)
                    for name in d:
                        dct[name] = (dist, group)
            self._entry_pt_classes = dct
        return self._entry_pt_classes
            
    def get_available_types(self, groups=None):
        """Return a set of tuples of the form (typename, dist_version), one
        for each available plugin type in the given entry point groups.
        If groups is None, return the set for all openmdao entry point groups.
        """
        ret = []
        
        if groups is None:
            groups = _plugin_groups
        groups = set(groups)
        
        for name, tup in self._entry_pt_classes.items():
            dist, group = tup
            if group in groups:
                ret.append((name, dist.version))
        return ret

