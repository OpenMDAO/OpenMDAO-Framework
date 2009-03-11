"""
Simple M4 DOE example.
"""

from openmdao.main import Assembly
from openmdao.main.component import RUN_OK

import m4.doe
import m4.test_components

import openmdao.main.factorymanager as factorymanager
from openmdao.main.importfactory import ImportFactory
factorymanager.register_factory(ImportFactory())

class Model(Assembly):
    """ Simple M4 DOE example.  """

    def __init__(self, name='M4_DOE_example', parent=None):
        super(Model, self).__init__(name, parent)

        # The model is just an M4 test component.
        self.workflow.add_node(m4.test_components.Model_A2d(parent=self))

        # Specify DOE.
        doe = m4.doe.DOE(parent=self)
        self.driver = doe

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


def main():
    """ Run model and print results. """
    model = Model()
    status = model.run()
    if status == RUN_OK:
        for i, case in enumerate(model.driver.outerator):
            print 'CASE %d:' % (i+1)
            for name, index, value in case.inputs:
                print '    input:', name, index, value
            if case.status == RUN_OK:
                for name, index, value in case.outputs:
                    print '    output:', name, index, value
            else:
                print '    FAILED, status = %d, msg: %s' % \
                      (case.status, case.msg)
    else:
        print 'Run failed, status', status


if __name__ == '__main__':
    main()

