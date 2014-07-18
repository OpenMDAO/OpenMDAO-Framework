
import time

import numpy as np

from openmdao.main.api import Assembly, dump_iteration_tree, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.mpiwrap import mpiprint, set_print_rank
from openmdao.main.test.test_derivatives import SimpleDriver

class Paraboloid(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """

    # set up interface to the framework
    # pylint: disable=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')

    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """

        x = self.x
        y = self.y

        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

    def provideJ(self):
        """Analytical first derivatives"""

        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x

        self.J = np.array([[df_dx, df_dy]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)
        return input_keys, output_keys


def _get_modelsimple():
    """simple 1 component test"""
    top = set_as_top(Assembly())
    top.add('comp', Paraboloid())
    top.add('driver', SimpleDriver())
    top.driver.workflow.add(['comp'])
    top.driver.add_parameter('comp.x', low=-1000, high=1000)
    top.driver.add_parameter('comp.y', low=-1000, high=1000)
    top.driver.add_objective('comp.f_xy')

    top.comp.x = 3
    top.comp.y = 5

    return top


if __name__ == '__main__':
    import sys
    import traceback
    from openmdao.main.mpiwrap import MPI

    """
    To run various tests, use the following cmdline:   mpirun -n <numprocs> python test_mpi.py --run <modelname>
    where modelname is whatever comes after _get_model in the various _get_model* functions above.
    """

    run = False
    mname = ''

    for arg in sys.argv[1:]:
        if arg.startswith('--run'):
            run = True
        elif arg.startswith('--rank'):
            set_print_rank(int(arg.split('=',1)[1]))
        elif not arg.startswith('-'):
            mname = arg

    ret = globals().get('_get_model%s' % mname)()
    if isinstance(ret, tuple):
        top, expected = ret
    else:
        top = ret
        expected = None

    #dump_iteration_tree(top)

    try:
        if not run:
            top._setup()
            mpiprint(top.driver.workflow._system.dump(stream=None))

            mpiprint("setup DONE")

        if run:
            mpiprint('-'*50)
            top.run()

            mpiprint('-'*50)

            mpiprint(top.driver.workflow._system.vec['df'].keys())

            J1 = top.driver.workflow.calc_gradient(mode='forward')
            mpiprint('Gradient - forward')
            mpiprint(J1)

            J2 = top.driver.workflow.calc_gradient(mode='adjoint')
            mpiprint('Gradient - adjoint')
            mpiprint(J2)

            if expected:
                mpiprint('-'*50)
                mpiprint("{0:<17} {1:<17} {2:<17} {3:<17}".format("Name",
                                                               "Expected",
                                                               "Actual",
                                                               "Error"))
                for name, expval in expected.items():
                    val = top.get(name)
                    err = expval - val
                    mpiprint("{0:<17} {1:<17} {2:<17} {3:<17}".format(name, expval, val, err))
    except Exception as err:
        mpiprint(traceback.format_exc())
