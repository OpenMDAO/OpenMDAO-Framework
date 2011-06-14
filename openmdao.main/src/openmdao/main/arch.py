
from zope.interface import implements

from openmdao.main.interfaces import IArchitecture

class Architecture(object): 
    implements(IArchitecture)
    
    def __init__(self, parent=None, param_types=None,
                 constraint_types=None, num_allowed_objectives=0,
                 has_coupling_vars=False):
        self.parent = parent
        self.param_types = param_types
        self.constraint_types = constraint_types
        self.num_allowed_objectives = num_allowed_objectives
        self.has_coupling_vars = has_coupling_vars
    
    def configure(self): 
        """setup the architecture inside of the assembly"""
        raise NotImplementedError("configure")
        
    def clear(self):
        raise NotImplementedError("clear")
    
    def check_config(self):
        """Check the current configuration and raise an exception if
        something's not right.
        """
        if self.parent is None:
            raise RuntimeError("no parent Assembly is defined for this Architecture")
        
        try:
            driver = self.parent.driver
        except AttributeError:
            driver = None
        if driver is None:
            raise RuntimeError("parent Assembly has no driver")
        
        if self.num_allowed_objectives > 0:
            try:
                maxobjs = self.parent.max_objectives()
            except:
                maxobjs = 0
            if maxobjs < self.num_allowed_objectives:
                raise RuntimeError("parent Assembly doesn't support the number "
                                   "of required objectives (%d)" % 
                                   self.num_allowed_objectives)
        
        