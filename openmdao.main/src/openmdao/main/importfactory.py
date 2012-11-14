
#public symbols
__all__ = ["ImportFactory"]

import sys

from openmdao.main.factory import Factory
from openmdao.util.log import logger


class ImportFactory(Factory):
    """Creates objects using the standard Python *__import__* mechanism. The 
    object must have a ctor with the same name as the module, minus the file 
    extension.  For example, to create a MyComp object, the module must be 
    named *MyComp.py* (or *.pyc* or *.pyo*). This factory does not support specific 
    version creation or creation on a remote server."""

    def __init__(self):
        super(ImportFactory, self).__init__()
        self._ctors = {}

    def create(self, typ, version=None, server=None, 
               res_desc=None, **ctor_args):
        """Tries to import the given named module and return a factory 
        function from it. The factory function or constructor must have the same
        name as the module. The module must be importable in the current Python
        environment.
        """
        if server is not None or version is not None:
            return None
        if res_desc is not None and len(res_desc)>0:
            return None
        ctor = self._import(typ)
        if ctor is None:
            return None
        else:
            return ctor(**ctor_args)

    def _import(self, typ):
        """Return class for *typ*."""
        if typ not in self._ctors:
            parts = typ.split('.')
            cname = parts[-1]
            modname = '.'.join(parts[:-1])
            try:
                __import__(modname, globals(), locals(), [cname])
                mod = sys.modules[modname]
            except (ImportError, KeyError) as err:
                logger.error(str(err))
                return None
            try:
                self._ctors[typ] = getattr(mod, cname)
            except AttributeError as err:
                logger.error(str(err))
                return None
        
        return self._ctors[typ]

    def get_available_types(self, groups=None):
        """Does nothing but is included to adhere to the Factory interface."""
        return []

    def get_signature(self, typname, version=None):
        """Return constructor argument signature for *typname,* using the
        specified package version. The return value is a dictionary.
        """
        if version is not None:
            return None
        cls = self._import(typname)
        if cls is None:
            return None
        else:
            return self.form_signature(cls)

