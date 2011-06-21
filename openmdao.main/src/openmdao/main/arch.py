
from zope.interface import implements

from openmdao.main.interfaces import IArchitecture
from openmdao.main.hasparameters import ParameterGroup

class Architecture(object):
    """Base class for classes that auto-configure an ArchitectureAssembly
    given a problem formulation based on parameters, constraints, objectives,
    and a model.
    """
    implements(IArchitecture)
    
    def __init__(self, parent=None, param_types=None,
                 constraint_types=None, num_allowed_objectives=None,
                 has_coupling_vars=False):
        self.parent = parent
        self.param_types = param_types
        self.constraint_types = constraint_types
        self.num_allowed_objectives = num_allowed_objectives
        self.has_coupling_vars = has_coupling_vars
    
    @property
    def param_types(self):
        """Types of parameters allowed by this Architecture."""
        return self.__param_types
    
    @param_types.setter
    def param_types(self, values):
        if values is None:
            self.__param_types = None
            return
        allowed = ['discrete','enum','continuous']
        diff = set(values).difference(allowed)
        if len(diff) > 0:
            raise ValueError("the following parameter types are invalid: %s."
                             " Allowed values are: %s" % (list(diff), allowed))
        self.__param_types = list(values)
        
    @property
    def constraint_types(self):
        """Types of constraints allowed by this Architecture."""
        return self.__constraint_types
    
    @constraint_types.setter
    def constraint_types(self, values):
        if values is None:
            self.__constraint_types = None
            return
        allowed = ['eq','ineq']
        diff = set(values).difference(allowed)
        if len(diff) > 0:
            raise ValueError("the following constraint types are invalid: %s."
                             " Allowed values are: %s" % (sorted(list(diff)), 
                                                          allowed))
        self.__constraint_types = list(values)
        
    def configure(self): 
        """setup the architecture inside of the assembly"""
        raise NotImplementedError("configure")
        
    def clear(self):
        raise NotImplementedError("clear")
    
    def _get_param_types(self):
        """Returns a list of parameter types that are currently present
        in the parent.  Possible entry values are: 'continuous', 'discrete', and 'enum'.
        """
        typeset = set()
        params = self.parent.get_parameters()
        for param in params.values():
            if isinstance(param, ParameterGroup):
                param = param._params[0]
            if param.valtypename in ['int','int32','int64']:
                typeset.add('discrete') 
            if param.vartypename == 'Enum':
                typeset.add('enum')
            # use elif here to prevent Enum with float values from being
            # treated as continuous
            elif param.valtypename in ['float','float32','float64']: 
                typeset.add('continuous')
        return list(typeset)
    
    def _get_constraint_types(self):
        typeset = set()
        if hasattr(self.parent, 'get_eq_constraints'):
            if len(self.parent.get_eq_constraints()) > 0:
                typeset.add('eq')
        if hasattr(self.parent, 'get_ineq_constraints'):
            if len(self.parent.get_ineq_constraints()) > 0:
                typeset.add('ineq')
        return typeset
    
    def check_config(self):
        """Check the current configuration and raise an exception if
        something's not right.
        """
        if self.parent is None:
            raise RuntimeError("no parent Assembly is defined for this Architecture")
        
        if self.num_allowed_objectives is not None:
            try:
                objs = self.parent.get_objectives()
            except:
                objs = {}
            if len(objs) > self.num_allowed_objectives:
                raise RuntimeError("this Architecture supports %d objectives, but "
                                   "%d were found in the parent" % 
                                   (self.num_allowed_objectives, len(objs)))
        
        if self.param_types is not None:
            try:
                parent_param_types = self._get_param_types()
            except AttributeError:
                parent_param_types = []
            diff = set(parent_param_types) - set(self.param_types)
            if len(diff) > 0:
                raise RuntimeError("this Architecture doesn't support the following "
                                   "parameter types: %s" % list(diff))
        
        if self.constraint_types is None and self._get_constraint_types(): 
            raise RuntimeError("this Architecture doesn't support any constraints")
            
        if self.constraint_types is not None:
            try:
                parent_cnstr_types = self._get_constraint_types()
            except AttributeError:
                parent_cnstr_types = []
            diff = set(parent_cnstr_types) - set(self.constraint_types)
            if len(diff) > 0:
                raise RuntimeError("this Architecture doesn't support the following "
                                   "constraint types: %s" % list(diff))
                
        try:
            parent_coupling_vars = self.parent.list_coupling_vars()
        except AttributeError:
            parent_coupling_vars = []
        if len(parent_coupling_vars) > 0 and not self.has_coupling_vars:
            raise RuntimeError("this Architecture doesn't support coupling variables")
        
    