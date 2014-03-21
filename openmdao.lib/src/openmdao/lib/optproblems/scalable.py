"""Scalable Test Optimization test problem proposed by Martins and Marriage.

J. R. R. A. Martins and C. Marriage, "An Object-Oriented Framework for
Multidisciplinary Design Optimization," 3rd AIAA Multidisciplinary
Design Optimization Specialist Conference, 2007. """

from openmdao.main.api import Component
from openmdao.main.problem_formulation import OptProblem

from openmdao.main.datatypes.api import Float, Array

from numpy import array, matrix, identity, zeros, ones


class Discipline_No_Deriv(Component):
    '''Single discipline for the scalable problem.'''

    c_y_out = Float(1.0, iotype="in",
                    desc="Coefficient for the output variables.")

    def __init__(self, prob_size=1):
        super(Discipline_No_Deriv, self).__init__()

        self.add_trait("z", Array(zeros((prob_size, 1)), iotype="in",
                                 desc="global variables",
                                 shape=(prob_size,1)))

        self.add_trait("C_z", Array(ones((prob_size, prob_size)), iotype="in",
                                   desc="global variable constants",
                                   shape=(prob_size, prob_size)))

        self.add_trait("x", Array(zeros((prob_size, 1)), iotype="in",
                                 desc="local variables",
                                 shape=(prob_size, 1)))

        self.add_trait("C_x", Array(ones((prob_size, prob_size)), iotype="in",
                                   desc="local variable constants",
                                   shape=(prob_size, prob_size)))

        #have to have the same number of coupling inputs as discipline outputs
        self.add_trait("y_out", Array(zeros((prob_size, 1)), iotype="out",
                                 desc="discipline output varaibles",
                                 shape=(prob_size, 1)))

        self.add_trait("y_in", Array(zeros((prob_size, 1)), iotype="in",
                                 desc="input coupling variables",
                                 shape=(prob_size, 1)))

        self.add_trait("C_y", Array(identity(prob_size), iotype="in",
                                   desc="local variable constants",
                                   shape=(prob_size, prob_size)))

    def execute(self):
        Cz = matrix(self.C_z)
        z = matrix(self.z)
        Cx = matrix(self.C_x)
        x = matrix(self.x)
        Cy = matrix(self.C_y)
        y = matrix(self.y_in)

        self.y_out = array(-1.0/self.c_y_out*(Cz*z + Cx*x - Cy*y))
        #print "running", self.name, z, x, self.y_in, self.y_out

class Discipline(Discipline_No_Deriv):
    '''Add derivatives to the scalable problem.'''

    def provideJ(self):
        """ Calculate the Jacobian """

        self.Jx = self.C_x/self.c_y_out
        self.Jy = self.C_y/self.c_y_out
        self.Jz = self.C_z/self.c_y_out

    def list_deriv_vars(self):
        return ('c_y_out', 'x', 'z', 'C_x', 'C_z', 'y_in', 'C_y'), ('y_out',)

    def apply_deriv(self, arg, result):
        """Multiply an input vector by the Jacobian."""

        if 'y_out' in result:

            if 'x' in arg:
                result['y_out'] -= self.Jx.dot(arg['x'])
            if 'y_in' in arg:
                result['y_out'] += self.Jy.dot(arg['y_in'])
            if 'z' in arg:
                result['y_out'] -= self.Jz.dot(arg['z'])


    def apply_derivT(self, arg, result):
        """Multiply an input vector by the Jacobian."""

        if 'y_out' in arg:

            if 'x' in result:
                result['x'] -= self.Jx.T.dot(arg['y_out'])
            if 'y_in' in result:
                result['y_in'] += self.Jy.T.dot(arg['y_out'])
            if 'z' in result:
                result['z'] -= self.Jz.T.dot(arg['y_out'])



class UnitScalableProblem(OptProblem):
    '''Model that contains N scalable discipline. Output is scaled so that the
    values are 1.0'''

    def __init__(self, n_disciplines=3, prob_size=3):
        """Initialize a model of the unit scalable problem.

        n_disciplines: int
            Number of components in the model

        prob_size: int
            Width of the coupling variables.
        """

        self.solution = {}

        self.disciplines = []

        self.n_disciplines = n_disciplines
        self.prob_size = prob_size

        super(UnitScalableProblem, self).__init__()

    def configure(self):
        """Set up the problem.
        """
        n_disciplines = self.n_disciplines
        prob_size = self.prob_size

        for i in range(0, n_disciplines):
            name = 'd%d' % i
            #each discipline as n_discipline-1 coupling vars
            d = self.add(name, Discipline(prob_size))
            self.disciplines.append(name)
            #scale to the number of disciplines to keep values of y at 1
            d.c_y_out = float(n_disciplines)

            #adding all local variables to the problem formulation
            for j in range(0, prob_size):
                self.add_parameter('%s.x[%d][0]'%(name, j), low=-10, high=10,
                                   start=-1.0)
                self.solution['%s.x[%d][0]' % (name, j)] = \
                    (1.0/float(n_disciplines))-1.0

            for j in range(0, prob_size):
                self.add_constraint("1-%s.y_out[%d][0] <= 0"%(name, j))


        #global variables
        for i in range(0, prob_size):
            params = tuple(["%s.z[%d][0]"%(name, i) for name in self.disciplines])
            self.add_parameter(params, low=-10, high=10, start=-1.0,
                               name="z%d"%i)
            self.solution["z%d" % i] = 0

        #coupling vars
        for i, j in zip(range(0, n_disciplines-1), range(1, n_disciplines)):
            for k in range(0, prob_size):
                self.add_coupling_var(("%s.y_in[%d][0]" % (self.disciplines[j], k),
                                       "%s.y_out[%d][0]" % (self.disciplines[i], k)),
                                      start = 0)
                self.solution[("%s.y_in[%d][0]" % (self.disciplines[j], k),
                               "%s.y_out[%d][0]" % (self.disciplines[i], k))] = 1.0
            #self.connect("%s.y_out"%(self.disciplines[i],),"%s.y_in"%(self.disciplines[j],))
        for k in range(0, prob_size):
            self.add_coupling_var(("%s.y_in[%d][0]" % (self.disciplines[0], k),
                                   "%s.y_out[%d][0]" % (self.disciplines[-1], k)),
                                  start = 0)
            self.solution[("%s.y_in[%d][0]" % (self.disciplines[0], k),
                           "%s.y_out[%d][0]" % (self.disciplines[-1], k))] = 1.0

        #objective
        parts = []
        for i in range(0, prob_size):
            #only need one target for each global
            parts.append('d0.z[%d][0]**2' % i)
        for d in self.disciplines:
            for j in range(0, prob_size):
                parts.append("%s.y_out[%d][0]**2" % (d, j))

        self.add_objective("+".join(parts), name="obj1")
        self.solution['obj1'] = float(n_disciplines)*prob_size


if __name__ == '__main__':


    from openmdao.lib.architectures.api import MDF, IDF

    sp = UnitScalableProblem(3)

    sp.architecture = IDF()

    sp.run()

    error = sp.check_solution()


    print "solution"
    for k, v in sp.solution.iteritems():
        print k, v
    print 
    print "error"
    #print sp.d0.x0
    for k, v in error.iteritems():
        print k, v


    #print sum([v**2 for k,v in error.iteritems()])**.5



