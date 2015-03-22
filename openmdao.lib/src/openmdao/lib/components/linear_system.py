import numpy as np

from openmdao.main.api import ImplicitComponent
from openmdao.lib.datatypes.api import Array

class LinearSystem(ImplicitComponent):
    """Solves the linear system Ax=b for x"""

    A = Array(iotype="in")
    b = Array(iotype="in")

    x = Array(iotype="state")
    R = Array(iotype="residual", desc='residuals for the linear system')

    def __init__(self, system_size):
        super(LinearSystem, self).__init__()
        self.eval_only = False
        self.system_size = system_size
        self.x = np.zeros(system_size)
        self.R = np.zeros(system_size)

    def solve(self):
        '''solve the full linear system'''
        self.x = np.linalg.solve( self.A, self.b )

    def evaluate(self):
        '''compute the residual'''

        self.R = self.A.dot(self.x) - self.b

    def list_deriv_vars(self):
        #return ('A', 'b', 'x'),('R',)
        return ('A','x','b'),('R',)

    def provideJ(self):
        pass

    def apply_deriv(self, arg, result):
        system_size = self.system_size
        if 'A' in arg:
            arg_A = arg['A']
            for i in xrange(system_size):
                result['R'][i] += np.sum(self.x*arg_A[i])
        if 'b' in arg:
            arg_b = arg['b']
            result['R'] -= arg_b
        if 'x' in arg:
            arg_x = arg['x']
            result['R'] += self.A.dot(arg_x)



    def apply_derivT(self, arg, result):

        system_size = self.system_size
        arg_R = arg['R']
        if 'A' in result: #TODO, vectorize this somehow
            for k in xrange(system_size**2):
                i,j = k/system_size, k%system_size
                result['A'][i,j] += arg_R[i]*self.x[j]
        if 'b' in result:
            result['b'] -= arg_R
        if 'x' in result:
            result['x'] += self.A.T.dot(arg_R)


if __name__ == "__main__":


    ls = LinearSystem(3)
    ls.A = np.array([[3,2,-1],[2,-2,4],[-1,.5,-1]], dtype="float")
    ls.b = np.array([1.,-2.,0.], dtype="float")
    ls.run()

    J = np.zeros((3,9))

    for i in xrange(9):
        arg_A_flat = np.zeros(9)
        arg_A_flat[i] = 1
        arg_A = arg_A_flat.reshape((3,3))

        arg = {'A':arg_A}
        result = {'R':np.zeros(3)}
        ls.apply_deriv(arg, result )
        J[:,i] = result['R']

    print J
