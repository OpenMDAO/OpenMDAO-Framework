from openmdao.main.api import Component, Assembly
from openmdao.main.datatypes.api import Float, Int, Array
from openmdao.lib.drivers.api import NewtonSolver
import numpy as np


class PolyScalableProblem(Assembly):
    """
    Multivariable polynomial test problem

    m : number of variables (also the number of components)

    X : vector of variables. will be set by the solver

    roots : vector of solutions for each variable.
            The output of each component will be zero when `X` takes the
            value of roots.
    """
    def __init__(self, m):
        self.m = m
        super(PolyScalableProblem, self).__init__()

    def configure(self):
        self.add("driver", NewtonSolver())
        self.add("X", Array(np.zeros(self.m), iotype="in"))
        self.add("roots", Array(np.zeros(self.m), iotype="in"))

        self.driver.add_parameter("X", low=-1e30, high=1e30)
        for i in xrange(self.m):
            compname = "p%s" % str(i)
            self.add(compname, PolyComp(self.m))
            self.driver.workflow.add(compname)
            self.connect("X", compname + ".X")
            self.connect("roots", compname + ".roots")
            self.driver.add_constraint("p%s.F = 0.0" % str(i))

class PolyComp(Component):
    """
    Single polynomial component.
    Constructs and evaluates a polynomial constructed from variables sampled
    from the input array, `X`
    The variables sampled from `X` can be set on instancing (by setting the
    indices into `X` in the list `idx`). Otherwise, they will be selected
    randomly.

    For example, if idx = [0,1], than the output F of this component is:

    F = (X[0]-roots[0]) + (X[1]-roots[1]) + (X[0]-roots[0])*(X[1]-roots[1])

    and will take the value of zero if the values of `X` match the corresponding
    `root` values.

    """
    F = Float(0., iotype="out")

    def __init__(self, m, idx = []):
        self.m = m
        self.idx = idx
        self.p = 1.
        super(PolyComp, self).__init__()

    def configure(self):
        self.add("X", Array(np.zeros(self.m), iotype="in"))
        self.add("roots", Array(np.zeros(self.m), iotype="in"))
        if not len(self.idx):
            num_vars = np.random.randint(2, self.m)
            self.idx = np.random.choice(range(self.m), num_vars, replace=False),

    def execute(self):
        self.F = np.sum((self.X[self.idx] - self.roots[self.idx]))
        self.F += np.prod(self.X[self.idx] - self.roots[self.idx])

    def provideJ(self):
        self.J = np.zeros((1,self.m))
        for i in xrange(m):
            if i in self.idx[0]:
                term = 1. + np.prod([self.X[k] - self.roots[k] for k in self.idx[0] if k !=i])
                self.J[0, i] = term
        return self.J

    def list_deriv_vars(self):
        input_keys = ('X',)
        output_keys = ('F',)
        return input_keys, output_keys


if __name__ == "__main__":
    m = 10 # number of polynomial variables & components
    A = PolyScalableProblem(m)
    A.roots = np.arange(m)
    # A.X = np.arange(m) # actual solution
    A.run()
    print A.X # computed solution
    print max([A.get("p%s" % str(i)).F for i in xrange(m)]) # max error