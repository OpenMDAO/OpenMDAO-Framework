""" Pseudo package containing all of the main classes/objects in the 
openmdao.main API.
"""

# NOTE: Order here is important!
#       Odd import errors could be the result of importing from a module
#       before importing from those modules it imports from.


from openmdao.main.log import logger
from openmdao.main.expreval import ExprEvaluator

from openmdao.main.factory import Factory
from openmdao.main.importfactory import ImportFactory
from openmdao.main.pkg_res_factory import PkgResourcesFactory

from openmdao.main.container import Container
from openmdao.main.component import Component
from openmdao.main.assembly import Assembly
from openmdao.main.driver import Driver
from openmdao.main.workflow import Workflow
from openmdao.main.dataflow import Dataflow

from openmdao.main.exceptions import ConstraintError

from openmdao.main.stringref import StringRef, StringRefArray

from openmdao.main.case import Case, FileCaseIterator, ListCaseIterator


__all__ = ['Container',
           'Component',
           'Assembly',
           'Driver',
           'Workflow',
           'ConstraintError',
           'Factory',
           'ImportFactory',
           'PkgResourcesFactory',
           'ExprEvaluator',
           'Case',
           'FileCaseIterator',
           'ListCaseIterator',]

