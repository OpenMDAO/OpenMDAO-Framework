
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



# This is a list of all of the entry point groups that OpenMDAO uses to
# identify plugins.
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
        self._have_new_types = True
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
            lst = classes[typ]
            dist = lst[0]
            groups = lst[1]
            klass = dist.load_entry_point(groups[0], typ)
            
            if version is not None and dist.version != version:
                return None
            
            return klass(**ctor_args)
        except KeyError:
            # try to look in the whole environment
            for group in self._groups:
                for proj in self.env:
                    for dist in self.env[proj]:
                        if version is not None and version != dist.version:
                            continue
                        ep = dist.get_entry_info(group, typ)
                        if ep is not None:
                            dist.activate()
                            klass = ep.load(require=True, env=self.env)
                            self._have_new_types = True
                            return klass(**ctor_args)
                        if version is None:
                            # newest version didn't have entry point, so skip to next project
                            break
        return None
            
    def _get_type_dict(self):
        if self._have_new_types:
            dct = {}
            for group in _plugin_groups:
                for dist in working_set:
                    d = dist.get_entry_map(group)
                    for name in d:
                        lst = dct.setdefault(name, [dist, []])
                        lst[1].append(group)
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
        
        typ_dict = self._get_type_dict()
        distset = set()
        for name, lst in typ_dict.items():
            dist = lst[0]
            distset.add(dist.project_name)
            for group in lst[1]:
                if group in groups:
                    ret.append((name, dist.version))
                
        # now look in the whole environment
        for group in groups:
            for proj in self.env:
                for dist in self.env[proj]:
                    if dist.project_name in distset:
                        break
                    for name in dist.get_entry_map(group):
                        ret.append((name, dist.version))
                            
        return ret

