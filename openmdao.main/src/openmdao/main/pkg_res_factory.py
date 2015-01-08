import copy
import logging

# these fail to find pkg_resources when run from pylint
# pylint: disable-msg=F0401
from pkg_resources import working_set, Environment

from openmdao.main.factory import Factory
from openmdao.util.dep import plugin_groups, find_module, \
                              PythonSourceTreeAnalyser


class PkgResourcesFactory(Factory):
    """A Factory that loads plugins using the pkg_resources API, which means
    it searches through egg info of distributions in order to find any entry
    point groups corresponding to openmdao plugin types, e.g.,
    openmdao.component, openmdao.variable, etc.
    """

    def __init__(self, groups=plugin_groups.keys(), search_path=None):
        super(PkgResourcesFactory, self).__init__()
        self._have_new_types = True
        self._groups = copy.copy(groups)
        self._search_path = search_path
        self.env = Environment(search_path)
        self.tree_analyser = PythonSourceTreeAnalyser()

    def create(self, typ, version=None, server=None,
               res_desc=None, **ctor_args):
        """Create and return an object of the given type, with
        optional name, version, server id, and resource description.
        """
        if server is not None or res_desc is not None:
            return None
        klass = self._load(typ, version)
        if klass is None:
            return None
        else:
            return klass(**ctor_args)

    def _load(self, typ, version):
        """Return class for *typ* and *version*."""
        classes = self._get_type_dict()
        try:
            lst = classes[typ]
            dist = lst[0]
            groups = lst[1]
            klass = dist.load_entry_point(groups[0], typ)
            if version is not None and dist.version != version:
                return None
            return klass
        except KeyError:
            if self._search_path is None:
                return None
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
                            return klass
                        if version is None:
                            # newest version didn't have entry point, so skip to next project
                            break
        return None

    def _entry_map_info(self, distiter):
        dct = {}
        for group in plugin_groups.keys():
            for dist in distiter:
                for name, value in dist.get_entry_map(group).items():
                    lst = dct.setdefault(name, (dist, [], set()))
                    lst[1].append(group)
                    lst[2].add(value.module_name)
        return dct

    def _get_type_dict(self):
        if self._have_new_types:
            self._entry_pt_classes = self._entry_map_info(working_set)
        return self._entry_pt_classes

    def _get_meta_info(self, typ_list, groups, typ_dict):
        distset = set()
        for name, lst in typ_dict.items():
            dist = lst[0]
            modules = lst[2]
            distset.add(dist.project_name)

            if groups.intersection(lst[1]):
                ifaces = set()
                for g in lst[1]:
                    ifaces.update(plugin_groups[g])

                meta = {
                    'version': dist.version,
                    'ifaces': set(ifaces),
                }

                for modname in modules:
                    fpath = find_module(modname)
                    if fpath is not None:
                        fanalyzer = self.tree_analyser.analyze_file(fpath, use_cache=True)
                        # 'name' may be missing if that module only imports,
                        # does not define, the class.
                        if name in fanalyzer.classes:
                            meta['bases'] = fanalyzer.classes[name].bases
                            meta['ifaces'].update(fanalyzer.classes[name].meta['ifaces'])
                        else:
                            logging.warning('pkg_res_factory: %r not found', name)

                meta['ifaces'] = list(meta['ifaces'])
                typ_list.append((name, meta))
        self.tree_analyser.flush_cache()
        return distset

    def get_available_types(self, groups=None):
        """Return a set of tuples of the form (typename, dist_version), one
        for each available plugin type in the given entry point groups.
        If groups is None, return the set for all openmdao entry point groups.
        """
        ret = []

        if groups is None:
            groups = plugin_groups.keys()
        groups = set(groups)

        typ_dict = self._get_type_dict()
        self._get_meta_info(ret, groups, typ_dict)

        return ret

    def get_signature(self, typname, version=None):
        """Return constructor argument signature for *typname,* using the
        specified package version. The return value is a dictionary.
        """
        cls = self._load(typname, version)
        if cls is None:
            return None
        else:
            return self.form_signature(cls)
