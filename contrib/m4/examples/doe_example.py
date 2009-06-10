"""
Simple M4 DOE example.

This just shows how you can set a special driver (the example M4 DOE driver)
and use it to set component inputs and get component outputs.
The component in question just evaluates a simple expression.
"""

from openmdao.main import Assembly

import m4.doe
import m4.dummy_components

class MyModel(Assembly):
    """ Simple M4 DOE example.  """

    def __init__(self, name='M4_DOE_example', *args, **kwargs):
        super(MyModel, self).__init__(name, *args, **kwargs)

        # Specify DOE driver.
        doe = m4.doe.DOE(parent=self)

        # The model is just an M4 test component.
        doe.model = m4.dummy_components.Model_A2d(parent=self)

        doe.design_variables = [
            ('Model_A2d.x', 0., 5.),
            ('Model_A2d.y', 0., 5.)
        ]
        doe.response_variables = [
            ('Model_A2d.z1'),
            ('Model_A2d.z2')
        ]
        doe.type = 'rand_lhs'
        doe.n_samples = 200


# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

def main():
    """ Run model and print results. """
    model = MyModel()
    model.run()
    for i, case in enumerate(model.M4_DOE.outerator):
        print 'CASE %d:' % (i+1)
        for name, index, value in case.inputs:
            print '    input:', name, index, value
        if case.msg:
            print '    FAILED: %s' % case.msg
        else:
            for name, index, value in case.outputs:
                print '    output:', name, index, value


if __name__ == '__main__':
    main()

