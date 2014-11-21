from openmdao.main.api import Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.main.hasconstraints import HasConstraints, Has2SidedConstraints
from openmdao.main.interfaces import IHasParameters, implements, IHasConstraints, \
     IHasObjective, IHas2SidedConstraints
from openmdao.util.decorators import add_delegate

@add_delegate(HasParameters, HasObjective, HasConstraints,
              Has2SidedConstraints)
class SimpleDriver(Driver):
    """Driver with Parameters"""

    implements(IHasParameters, IHasConstraints, IHasObjective,
               IHas2SidedConstraints)

    def requires_derivs(self):
        return True