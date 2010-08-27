"""
Simple M4 DOE example.

This just shows how you can set a special driver (the example M4 DOE driver)
and use it to set component inputs and get component outputs.
The component in question just evaluates a simple expression.
"""

from openmdao.main.api import Assembly, set_as_top

import m4.doe
import m4.dummy_components

class MyModel(Assembly):
    """ Simple M4 DOE example.  """

    #name='M4_DOE_example'
    def __init__(self, *args, **kwargs):
        super(MyModel, self).__init__(*args, **kwargs)

        # Specify DOE driver.
        doe = self.add('M4_DOE', m4.doe.DOE())

        # The model is just an M4 test component.
        doe.add('model', m4.dummy_components.Model_A2d())

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


if __name__ == '__main__': # pragma no cover
    top = set_as_top(MyModel())
#    top.run()
    top.check_save_load()  # Note: requires correct pythonV.R

