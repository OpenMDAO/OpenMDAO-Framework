
from zope.interface import implements

from openmdao.main.container import Container
from openmdao.main.interfaces import IArchitecture, ICaseRecorder
from openmdao.main.hasparameters import ParameterGroup
from openmdao.main.datatypes.api import List, Slot


class Architecture(Container):
    """Base class for classes that auto-configure an ArchitectureAssembly
    given a problem formulation based on parameters, constraints, objectives,
    and a model.
    """
    implements(IArchitecture)

    data_recorders = List(Slot(ICaseRecorder, required=False),
                     desc='Case recorders for iteration data.')

    def __init__(self, parent=None, param_types=None,
                 constraint_types=None, num_allowed_objectives=None,
                 has_coupling_vars=False, has_global_des_vars=False):
        super(Architecture, self).__init__()

        self.parent = parent
        self.param_types = param_types
        self.constraint_types = constraint_types
        self.num_allowed_objectives = num_allowed_objectives
        self.has_coupling_vars = has_coupling_vars
        self.has_global_des_vars = has_global_des_vars
        self.configured = False

    @property
    def param_types(self):
        """Types of parameters allowed by this Architecture."""
        return self.__param_types

    @param_types.setter
    def param_types(self, values):
        if values is None:
            self.__param_types = None
            return
        allowed = ['discrete', 'enum', 'continuous']
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
        """Setup the architecture inside of the assembly."""
        raise NotImplementedError("configure")

    def _get_param_types(self):
        """Returns a list of parameter types that are currently present
        in the parent.  Possible entry values are: 'continuous', 'discrete',
        and 'enum'.
        """
        typeset = set()
        params = self.parent.get_parameters()
        for name, param in params.items():
            if isinstance(param, ParameterGroup):
                param = param._params[0]
            if param.valtypename in ['int', 'int32', 'int64']:
                typeset.add('discrete')
            # check Enum here to prevent Enum with float values from being
            # treated as continuous
            elif param.vartypename == 'Enum':
                typeset.add('enum')
            elif param.valtypename in ['float', 'float32', 'float64']:
                typeset.add('continuous')
            else:
                raise TypeError('unexpected type for parameter %s: %s (%s)'
                                % (name, param.vartypename, param.valtypename))
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


        try:
            lenobjs = len(self.parent.get_objectives())
        except:
            lenobjs = 0
        if lenobjs > 0:
            if self.num_allowed_objectives is None:
                raise RuntimeError("this Architecture doesn't support "
                                   "objectives, but %d were found in the parent"
                                   % lenobjs)
            elif lenobjs > self.num_allowed_objectives:
                raise RuntimeError("this Architecture supports %d objectives, "
                                   "but %d were found in the parent" %
                                   (self.num_allowed_objectives, lenobjs))

        try:
            parent_param_types = self._get_param_types()
        except AttributeError:
            parent_param_types = []
        if len(parent_param_types) > 0:
            if self.param_types is None:
                raise RuntimeError("this Architecture doesn't support "
                                   "parameters, but parameter types %s were "
                                   "found in parent" % parent_param_types)
            else:
                diff = set(parent_param_types) - set(self.param_types)
                if len(diff) > 0:
                    raise RuntimeError("this Architecture doesn't support the "
                                       "following parameter types: %s"
                                       % list(diff))
        try:
            parent_cnstr_types = self._get_constraint_types()
        except AttributeError:
            parent_cnstr_types = []
        if len(parent_cnstr_types) > 0:
            if self.constraint_types is None:
                raise RuntimeError("this Architecture doesn't support constraints")
            else:
                diff = set(parent_cnstr_types) - set(self.constraint_types)
                if len(diff) > 0:
                    raise RuntimeError("this Architecture doesn't support the "
                                       "following constraint types: %s"
                                       % list(diff))
        try:
            parent_coupling_vars = self.parent.list_coupling_vars()
        except AttributeError:
            parent_coupling_vars = []
        if len(parent_coupling_vars) > 0 and not self.has_coupling_vars:
            raise RuntimeError("this Architecture doesn't support coupling variables")

        try:
            parent_global_vars = self.parent.get_global_des_vars()
        except AttributeError:
            parent_global_vars = []
        if (not parent_global_vars) and self.has_global_des_vars:
            raise RuntimeError("this Architecture requires global design "
                               "variables in the problem formulation but none "
                               "were found in parent")

