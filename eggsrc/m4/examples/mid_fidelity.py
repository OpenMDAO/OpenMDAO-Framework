"""
Simple M4 variable fidelity example.
"""

from openmdao.main import Assembly, Float
from openmdao.main.variable import INPUT, OUTPUT

from m4.doe import DOE
from m4.mid_fidelity import MidFidelity 
from m4.test_components import Model_A2d, Model_B2d

import openmdao.main.factorymanager as factorymanager
from openmdao.main.importfactory import ImportFactory
factorymanager.register_factory(ImportFactory())

class Model(Assembly):
    """ Simple M4 variable fidelity example.  """

    def __init__(self, name='M4_VarFi', parent=None):
        super(Model, self).__init__(name, parent)

        # The model is an M4 variable fidelity component.
        var_fi = VarFi(parent=self)
        self.workflow.add_node(var_fi)

        # Specify DOE.
        doe = DOE(parent=self)
        self.driver = doe

        doe.design_variables = [
            (var_fi.name+'.x', 0., 5.),
            (var_fi.name+'.y', 0., 5.)
        ]
        doe.response_variables = [
            (var_fi.name+'.z1'),
            (var_fi.name+'.z2')
        ]
        doe.type = 'rand_lhs'
        doe.n_samples = 200


class VarFi(MidFidelity):
    """ Example variable fidelity component. """

    def __init__(self, name='VarFi', parent=None):
        super(VarFi, self).__init__(name, parent)

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

        # Input mappings (mid, lo, hi).
        Float('x', self, INPUT, default=0., min_limit=0., max_limit=5.,
              desc='X input value.')
        self.add_input_mapping('x', 'x', 'x')

        Float('y', self, INPUT, default=0., min_limit=0., max_limit=5.,
              desc='Y input value.')
        self.add_input_mapping('y', 'y', 'y')

        # Output mappings (mid, lo, hi).
        Float('z1', self, OUTPUT, default=0.,
              desc='exp(x) + exp(y)')
        self.add_output_mapping('z1', 'z', 'z1')

        Float('z2', self, OUTPUT, default=0.,
              desc='10.0*(x-2.0)**2 + 10.0*(y-1.5)**2 + 10.0')
        self.add_output_mapping('z2', 'z', 'z2')


if __name__ == '__main__':
    Model().run()

