# pylint: disable-msg=C0111,C0103

import unittest

# pylint: disable-msg=F0401,E0611
from openmdao.main.api import Assembly, set_as_top

from openmdao.main.uncertain_distributions import NormalDistribution

from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.surrogatemodels.api import ResponseSurface, \
                  KrigingSurrogate, FloatKrigingSurrogate, LogisticRegression

from openmdao.util.testutil import assert_rel_error


class MetaModelTestCase(unittest.TestCase):

    def test_basics(self):

        model = set_as_top(Assembly())
        model.add('meta', MetaModel(params=('x1', 'x2'),
                                    responses=('y1', 'y2')))
        model.driver.workflow.add('meta')

        model.meta.params.x1 = [1.0, 2.0, 3.0]
        model.meta.params.x2 = [1.0, 3.0, 4.0]
        model.meta.responses.y1 = [3.0, 2.0, 1.0]
        model.meta.responses.y2 = [1.0, 4.0, 7.0]

        model.meta.default_surrogate = ResponseSurface()

        model.meta.x1 = 2.0
        model.meta.x2 = 3.0
        model.meta.run()
        assert_rel_error(self, model.meta.y1, 2.0, .00001)
        assert_rel_error(self, model.meta.y2, 4.0, .00001)

        model.meta.x1 = 2.5
        model.meta.x2 = 3.5
        model.meta.run()
        assert_rel_error(self, model.meta.y1, 1.5934, .001)

        # Slot a new surrogate.
        model.meta.default_surrogate = FloatKrigingSurrogate()

        self.assertTrue(model.meta._train == True)
        model.meta.run()
        assert_rel_error(self, model.meta.y1, 1.4609, .001)

    # Array param not supported yet. - KTM
    #def test_array_inputs(self):

        #model = set_as_top(Assembly())
        #model.add('meta', MetaModel(params=('x',),
                                    #responses=('y1', 'y2')))
        #model.driver.workflow.add('meta')

        #model.meta.params.x = [[1.0, 1.0, 1.0, 1.0],
                               #[2.0, 1.0, 1.0, 1.0],
                               #[1.0, 2.0, 1.0, 1.0],
                               #[1.0, 1.0, 2.0, 1.0],
                               #[1.0, 1.0, 1.0, 2.0]]
        #model.meta.responses.y1 = [3.0, 2.0, 1.0, 6.0, -2.0]
        #model.meta.responses.y2 = [1.0, 4.0, 7.0, -3.0, 3.0]

        #model.meta.default_surrogate = ResponseSurface()

        #model.meta.x1 = [[1.0, 2.0], [1.0, 1.0]]
        #model.meta.run()
        #assert_rel_error(self, model.meta.y1, 1.0, .00001)
        #assert_rel_error(self, model.meta.y2, 7.0, .00001)

        #model.meta.x1 = 2.5
        #model.meta.x2 = 3.5
        #model.meta.run()
        #assert_rel_error(self, model.meta.y1, 1.5934, .001)

        ## Slot a new surrogate.
        #model.meta.default_surrogate = FloatKrigingSurrogate()

        #self.assertTrue(model.meta._train == True)
        #model.meta.run()
        #assert_rel_error(self, model.meta.y1, 1.4609, .001)

    def test_warm_start(self):

        model = set_as_top(Assembly())
        model.add('meta', MetaModel(params=('x1', 'x2'),
                                    responses=('y1', 'y2')))
        model.driver.workflow.add('meta')
        model.meta.default_surrogate = ResponseSurface()
        model.meta.warm_restart = True

        model.meta.params.x1 = [1.0, 3.0]
        model.meta.params.x2 = [1.0, 4.0]
        model.meta.responses.y1 = [3.0, 1.0]
        model.meta.responses.y2 = [1.0, 7.0]

        model.meta.default_surrogate = ResponseSurface()

        model.meta.x1 = 2.0
        model.meta.x2 = 3.0
        model.meta.run()
        assert_rel_error(self, model.meta.y1, 1.9085, .001)
        assert_rel_error(self, model.meta.y2, 3.9203, .001)

        # Adding the 3rd point moves the estimate for that point
        # back to where it should be.

        model.meta.params.x1 = [2.0]
        model.meta.params.x2 = [3.0]
        model.meta.responses.y1 = [2.0]
        model.meta.responses.y2 = [4.0]

        self.assertTrue(model.meta._train == True)
        model.meta.run()
        assert_rel_error(self, model.meta.y1, 2.0, .00001)
        assert_rel_error(self, model.meta.y2, 4.0, .00001)

    def test_multi_surrogate_models_bad_surrogate_dict(self):

        model = set_as_top(Assembly())
        model.add('meta', MetaModel(params=('x1', 'x2'),
                                    responses=('y1', 'y2')))
        model.driver.workflow.add('meta')
        model.meta.warm_restart = True

        model.meta.params.x1 = [1.0, 3.0]
        model.meta.params.x2 = [1.0, 4.0]
        model.meta.responses.y1 = [3.0, 1.0]
        model.meta.responses.y2 = [1.0, 7.0]

        model.meta.surrogates['y1'] = KrigingSurrogate()
        try:
            model.meta.run()
        except RuntimeError as err:
            self.assertEqual("meta: No default surrogate model is defined and "
                             "the following outputs do not have a surrogate "
                             "model: ['y2']. Either specify default_surrogate, "
                             "or specify a surrogate model for all outputs.",
                             str(err))
        else:
            self.fail('ValueError expected')

    def test_multi_surrogate_models(self):

        model = set_as_top(Assembly())
        model.add('meta', MetaModel(params=('x1', 'x2'),
                                    responses=('y1', 'y2')))
        model.driver.workflow.add('meta')
        model.meta.warm_restart = True

        model.meta.params.x1 = [1.0, 3.0]
        model.meta.params.x2 = [1.0, 4.0]
        model.meta.responses.y1 = [3.0, 1.0]
        model.meta.responses.y2 = [1.0, 7.0]

        model.meta.surrogates['y1'] = KrigingSurrogate()
        model.meta.surrogates['y2'] = LogisticRegression()

        model.meta.x1 = 2.0
        model.meta.x2 = 3.0
        model.meta.run()
        self.assertTrue(isinstance(model.meta.y1, NormalDistribution))
        self.assertTrue(isinstance(model.meta.y2, float))

        model.meta.surrogates['y1'] = LogisticRegression()
        model.meta.surrogates['y2'] = KrigingSurrogate()

        model.meta.run()
        self.assertTrue(isinstance(model.meta.y1, float))
        self.assertTrue(isinstance(model.meta.y2, NormalDistribution))

        model.meta.default_surrogate = KrigingSurrogate()
        model.meta.surrogates['y1'] = None
        model.meta.surrogates['y2'] = None

        model.meta.run()
        self.assertTrue(isinstance(model.meta.y1, NormalDistribution))
        self.assertTrue(isinstance(model.meta.y2, NormalDistribution))

        model.meta.surrogates['y1'] = LogisticRegression()

        model.meta.run()
        self.assertTrue(isinstance(model.meta.y1, float))
        self.assertTrue(isinstance(model.meta.y2, NormalDistribution))


if __name__ == "__main__":
    unittest.main()


