"""
Simple M4 variable fidelity example.

Runs a DOE on a MidFidelity component instance.
Note that the MidFidelity component performs its correction calculations
on the first execution, then on subsequent executions the corrected result
is directly calculated.
"""

from openmdao.main.api import Assembly
from openmdao.main.datatypes.api import Float

from m4.doe import DOE
from m4.mid_fidelity import MidFidelity 
from m4.dummy_components import Model_A2d, Model_B2d


class MyModel(Assembly):
    """ Simple M4 variable fidelity example.  """

    def configure(self):

        # Specify DOE.
        doe = self.add('M4_DOE', DOE())

        # The model is an M4 variable fidelity component.
        doe.model = self.add('VarFi', VarFi())

        doe.design_variables = [('x', 0., 5.), ('y', 0., 5.)]
        doe.response_variables = [('z1'), ('z2')]
        doe.type = 'rand_lhs'
        doe.n_samples = 200

    def execute(self):
        """ Run model and print results. """
        super(MyModel, self).execute()
        for i, case in enumerate(self.M4_DOE.outerator):
            print 'CASE %d:' % (i+1)
            for name, index, value in case.inputs:
                print '    input:', name, index, value
            if case.msg:
                print '    FAILED: %s' % case.msg
            else:
                for name, index, value in case.outputs:
                    print '    output:', name, index, value


class VarFi(MidFidelity):
    """ Example variable fidelity component. """

    # Inputs.
    x = Float(value=0., low=0., high=5., iotype='in', desc='X input value.')
    y = Float(default_value=0., low=0., high=5., units='m', iotype='in',
                   desc='Y input value.')

    # Outputs.
    z1 = Float(0., iotype='out', desc='exp(x) + exp(y)')
    z2 = Float(0., iotype='out',
               desc='10.0*(x-2.0)**2 + 10.0*(y-1.5)**2 + 10.0')
        
    def __init__(self):
        super(VarFi, self).__init__()

        # Inputs.
        self.rs_type = 'rbf'
        self.n_samples = 20
        self.tolerance = 1.0e20
        self.correction_function = 2  # additive(gamma)
        self.w_h = 0.2
        self.accuracy_test_type = 2   # additional-points

        # High and low fidelity models.
        self.set_hifi_model(Model_A2d())
        self.set_lofi_model(Model_B2d())

        # Mappings are (mid, low, high).
        self.add_input_mapping('x', 'x', 'x')
        self.add_input_mapping('y', 'y', 'y')
        self.add_output_mapping('z1', 'z', 'z1')
        self.add_output_mapping('z2', 'z', 'z2')


if __name__ == '__main__': # pragma no cover
    top = MyModel()
    top.run()
#    top.check_save_load()  # Note: requires correct pythonV.R

