"""
Two discipline components.
From Sellar's analytic problem.

    Sellar, R. S., Batill, S. M., and Renaud, J. E., "Response Surface Based, Concurrent Subspace
    Optimization for Multidisciplinary System Design," Proceedings References 79 of the 34th AIAA
    Aerospace Sciences Meeting and Exhibit, Reno, NV, January 1996.
"""

import numpy

from openmdao.main.api import Component
from openmdao.main.problem_formulation import OptProblem
from openmdao.main.datatypes.api import Float

class Discipline1(Component):
    """Component containing Discipline 1."""

    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable.')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable.')
    x1 = Float(1.0, iotype='in', desc='Local Design Variable.')
    y2 = Float(1.0, iotype='in', desc='Disciplinary Coupling.')

    y1 = Float(iotype='out', desc='Output of this Discipline.')


    def execute(self):
        """Evaluates the equation
        y1 = z1**2 + z2 + x1 - 0.2*y2"""

        z1 = self.z1
        z2 = self.z2
        x1 = self.x1
        y2 = self.y2

        self.y1 = z1**2 + z2 + x1 - 0.2*y2
        #print "%s (z1: %f, z2: %f, x1: %f, y2: %f, out(y1): %f)" % (self.name, z1, z2, x1, y2, self.y1)


class Discipline1_WithDerivatives(Component):
    """Component containing Discipline 1."""

    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable.')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable.')
    x1 = Float(0.0, iotype='in', desc='Local Design Variable.')
    y2 = Float(1.0, iotype='in', desc='Disciplinary Coupling.')

    y1 = Float(iotype='out', desc='Output of this Discipline.')

    def execute(self):
        """Evaluates the equation
        y1 = z1**2 + z2 + x1 - 0.2*y2."""

        z1 = self.z1
        z2 = self.z2
        x1 = self.x1
        y2 = self.y2

        self.y1 = z1**2 + z2 + x1 - 0.2*y2
        #print "%s (z1: %f, z2: %f, x1: %f, y2: %f, out(y1): %f)" % (self.name, z1, z2, x1, y2, self.y1)

    def provideJ(self):
        """ Calculate the Jacobian """

        J = numpy.zeros([1, 4])

        J[0, 0] = 1.0
        J[0, 1] = -0.2
        J[0, 2] = 2.0*self.z1
        J[0, 3] = 1.0

        return J

    def list_deriv_vars(self):
        input_keys = ('x1', 'y2', 'z1', 'z2')
        output_keys = ('y1',)

        return input_keys, output_keys

class Discipline2(Component):
    """Component containing Discipline 2."""

    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable.')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable.')
    y1 = Float(1.0, iotype='in', desc='Disciplinary Coupling.')

    y2 = Float(iotype='out', desc='Output of this Discipline.')


    def execute(self):
        """Evaluates the equation
        y2 = y1**(.5) + z1 + z2"""

        z1 = self.z1
        z2 = self.z2

        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will
        # throw it out
        y1 = abs(self.y1)

        self.y2 = y1**(.5) + z1 + z2
        #print "%s (z1: %f, z2: %f, y1: %f, out(y2): %f)" % (self.name, z1, z2, y1, self.y2)


class Discipline2_WithDerivatives(Component):
    """Component containing Discipline 2."""

    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable.')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable.')
    y1 = Float(1.0, iotype='in', desc='Disciplinary Coupling.')

    y2 = Float(iotype='out', desc='Output of this Discipline.')

    def execute(self):
        """Evaluates the equation
        y2 = y1**(.5) + z1 + z2."""

        z1 = self.z1
        z2 = self.z2

        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will
        # throw it out
        y1 = abs(self.y1)

        self.y2 = y1**(.5) + z1 + z2
        #print "Dis 2 (%f, %f, %f, out = %f)" % (z1, z2, y1, self.y2)

    def provideJ(self):
        """ Calculate the Jacobian """

        self.J = numpy.zeros([1, 3])

        self.J[0, 0] = .5*(abs(self.y1))**-0.5
        self.J[0, 1] = 1.0
        self.J[0, 2] = 1.0
        return self.J

    def list_deriv_vars(self):
        input_keys = ('y1', 'z1', 'z2')
        output_keys = ('y2',)

        return input_keys, output_keys

class SellarProblem(OptProblem):
    """ Sellar test problem definition."""

    def configure(self):
        """
        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        #add the discipline components to the assembly
        self.add('dis1', Discipline1())
        self.add('dis2', Discipline2())

        #START OF MDAO Problem Definition
        #Global Des Vars
        self.add_parameter(("dis1.z1","dis2.z1"),name="z1",low=-10,high=10,start=5.0)
        self.add_parameter(("dis1.z2","dis2.z2"),name="z2",low=0,high=10,start=2.0)

        #Local Des Vars
        self.add_parameter("dis1.x1",low=0,high=10,start=1.0)

        #Coupling Vars
        self.add_coupling_var(("dis2.y1","dis1.y1"),name="y1",start=1.0)
        self.add_coupling_var(("dis1.y2","dis2.y2"),name="y2",start=1.0)

        self.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)',name="obj1")
        self.add_constraint('dis1.y1 > 3.16')
        self.add_constraint('dis2.y2 < 24.0')

        #solution to the opt problem
        self.solution = {
            "z1":1.9776,
            "z2":0.0,
            "dis1.x1":0.0,
            "y1":3.16,
            "y2": 3.756,
            'obj1':3.1834
        }

        #END OF MDAO Problem Definition


class SellarProblemWithDeriv(OptProblem):
    """ Sellar test problem definition using components analytical derivatives."""

    def configure(self):
        """ Creates a new Assembly with this problem.

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        #add the discipline components to the assembly
        self.add('dis1', Discipline1_WithDerivatives())
        self.add('dis2', Discipline2_WithDerivatives())

        #START OF MDAO Problem Definition
        #Global Des Vars
        self.add_parameter(("dis1.z1","dis2.z1"),name="z1",low=-10,high=10,start=5.0)
        self.add_parameter(("dis1.z2","dis2.z2"),name="z2",low=0,high=10,start=2.0)

        #Local Des Vars
        self.add_parameter("dis1.x1",low=0,high=10,start=1.0)

        #Coupling Vars
        self.add_coupling_var(("dis2.y1","dis1.y1"),name="y1",start=1.0)
        self.add_coupling_var(("dis1.y2","dis2.y2"),name="y2",start=1.0)

        self.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)',name="obj1")
        self.add_constraint('3.16 < dis1.y1')
        self.add_constraint('dis2.y2 < 24.0')

        #solution to the opt problem
        self.solution = {
            "z1":1.9776,
            "z2":0.0,
            "dis1.x1":0.0,
            "y1":3.16,
            "y2": 3.756,
            'obj1':3.1834
        }

        #END OF MDAO Problem Definition
