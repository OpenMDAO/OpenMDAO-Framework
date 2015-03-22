"""
Pseudo package containing all of the main classes/objects in the
openmdao.main API.

"""

from openmdao.util.log import logger, enable_console
from openmdao.main.expreval import ExprEvaluator

from openmdao.main.factory import Factory
from openmdao.main.factorymanager import create, get_available_types

from openmdao.main.container import Container, get_default_name, \
                                    create_io_traits
from openmdao.main.vartree import VariableTree
from openmdao.main.component import Component, SimulationRoot
from openmdao.main.implicitcomp import ImplicitComponent
from openmdao.main.component_with_derivatives import ComponentWithDerivatives
from openmdao.main.driver_uses_derivatives import DriverUsesDerivatives
from openmdao.main.assembly import Assembly, set_as_top, dump_iteration_tree
from openmdao.main.driver import Driver
from openmdao.main.workflow import Workflow
from openmdao.main.variable import Variable

from openmdao.main.exceptions import ConstraintError

from openmdao.main.interfaces import implements, Attribute, Interface

from openmdao.main.file_supp import FileMetadata

from openmdao.main.case import Case

from openmdao.main.arch import Architecture
from openmdao.main.problem_formulation import ArchitectureAssembly, OptProblem

from openmdao.util.eggsaver import SAVE_PICKLE, SAVE_CPICKLE #, SAVE_YAML, SAVE_LIBYAML

from openmdao.units import convert_units


# TODO: This probably shouldn't be here. Removing it will require edits to some
# of our plugins
from openmdao.main.datatypes.slot import Slot
