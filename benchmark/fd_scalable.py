"""
A component with a large number of inputs is finite differenced.
"""

import numpy as np

from openmdao.lib.optproblems.scalable import Discipline
from openmdao.main.api import Assembly, Component, set_as_top

N = 100
np.random.seed(12345)

class Model(Assembly):

    def configure(self):

        self.add('comp', Discipline(prob_size=N))
        self.comp.C_y = np.random.random((N, N))

if __name__ == "__main__":

    from time import time

    top = set_as_top(Model())
    top.run()

    inputs = ['comp.y_in']
    outputs = ['comp.y_out']
    inputs = ['comp.y_in[%d, 0]'%n for n in range(N)]
    outputs = ['comp.y_out[%d, 0]'%n for n in range(N)]

    t0 = time()
    J = top.driver.calc_gradient(inputs=inputs,
                                 outputs=outputs,
                                 mode = 'fd')
    print 'Time elapsed', time() - t0


    # python -m cProfile -s time fd_scalable.py >z