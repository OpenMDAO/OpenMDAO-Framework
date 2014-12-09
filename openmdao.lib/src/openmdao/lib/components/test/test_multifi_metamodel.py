# pylint: disable-msg=C0111,C0103
import unittest
import mock

# pylint: disable-msg=F0401,E0611
from openmdao.main.api import Assembly, set_as_top

from openmdao.lib.components.api import MultiFiMetaModel

from openmdao.util.testutil import assert_rel_error
from openmdao.main.interfaces import implements, ISurrogate, IMultiFiSurrogate
from openmdao.main.api import Container

class MockSurrogate(Container):    
    implements(ISurrogate, IMultiFiSurrogate)
    
    def __init__(self):
        super(MockSurrogate, self).__init__()
        
    def train(self, X, Y):
        pass
    
    def train_multifi(self, X, Y):
        pass
    
    def get_uncertain_value(self, value):
        return 0.0
    
    def predict(self, x):
        return 0.0


class MultiFiMetaModelTestCase(unittest.TestCase):

    def test_inputs_wrt_nfidelity(self):
        model = set_as_top(Assembly())
        model.add('meta', MultiFiMetaModel(params=('x', ),
                                           responses=('y', ), nfi=3))
        model.driver.workflow.add('meta')
        
        self.assertEqual(model.meta.params.x, [])
        self.assertEqual(model.meta.params.x_fi2, [])
        self.assertEqual(model.meta.params.x_fi3, [])
        self.assertEqual(model.meta.responses.y, [])
        self.assertEqual(model.meta.responses.y_fi2, [])
        self.assertEqual(model.meta.responses.y_fi3, [])


    def test_one_dim_one_fidelity_training(self):
        
        model = set_as_top(Assembly())
        model.add('meta', MultiFiMetaModel(params=('x', ),
                                           responses=('y', )))
        model.driver.workflow.add('meta')

        model.meta.params.x = [0.0, 0.4, 1.0]
        model.meta.responses.y = [3.02720998, 0.11477697, 15.82973195]

        mock_surr = MockSurrogate()
        mock_surr.train   = mock.Mock()
        mock_surr.predict = mock.Mock(return_value = 0.0)
        
        model.meta.surrogates['y'] = mock_surr        
        model.meta.run()
                
        expected_xtrain=[ [0.0], [0.4], [1.0] ]
        expected_ytrain=[ 3.02720998, 0.11477697, 15.82973195 ]               
        mock_surr.train.assert_called_with(expected_xtrain, expected_ytrain)        

        model.meta.x = 0.5
        model.meta.run()
        mock_surr.predict.assert_called_with([0.5])
        
      
    def test_one_dim_bi_fidelity_training(self):

        model = set_as_top(Assembly())
        model.add('meta', MultiFiMetaModel(params=('x', ),
                                           responses=('y', ), nfi=2))
        model.driver.workflow.add('meta')

        model.meta.params.x        = [0.0, 0.4, 1.0]
        model.meta.params.x_fi2    = [0.1, 0.2, 0.3, 0.5, 0.6, 
                                      0.7, 0.8, 0.9, 0.0, 0.4, 1.0]
        model.meta.responses.y     = [3.02720998, 0.11477697, 15.82973195]
        model.meta.responses.y_fi2 = [-9.32828839, -8.31986355, -7.00778837, 
                                      -4.54535129, -4.0747189 , -5.30287702, 
                                      -4.47456522, 1.85597517, -8.48639501, 
                                      -5.94261151, 7.91486597]
        mock_surr = MockSurrogate()
        mock_surr.train_multifi = mock.Mock()
        model.meta.surrogates['y'] = mock_surr   
        model.meta.run()
        expected_xtrain=[[[0.0], [0.4], [1.0]], 
                         [[0.1], [0.2], [0.3], [0.5], [0.6], [0.7], 
                          [0.8], [0.9], [0.0], [0.4], [1.0]]]
        expected_ytrain=[[  3.02720998, 0.11477697, 15.82973195], 
                         [-9.32828839, -8.31986355, -7.00778837, -4.54535129, 
                          -4.0747189, -5.30287702, -4.47456522,  1.85597517, 
                          -8.48639501, -5.94261151,  7.91486597]]        
        mock_surr.train_multifi.assert_called_with(expected_xtrain, expected_ytrain)                
  
    def test_two_dim_bi_fidelity_training(self):
        model = set_as_top(Assembly())
        model.add('meta', MultiFiMetaModel(params=('x1', 'x2' ),
                                           responses=('y1', 'y2' ), nfi=2))
        model.driver.workflow.add('meta')

        model.meta.params.x1        = [1.0, 2.0, 3.0]
        model.meta.params.x1_fi2    = [1.1, 2.1, 3.1, 1.0, 2.0, 3.0]
        model.meta.params.x2        = [1.0, 2.0, 3.0]
        model.meta.params.x2_fi2    = [2.1, 2.2, 2.3, 1.0, 2.0, 3.0]
        model.meta.responses.y1     = [0.0, 0.1, 0.2]
        model.meta.responses.y1_fi2 = [3.0, 3.1, 3.3, 3.4, 3.5 ,3.6]
        model.meta.responses.y2     = [4.0, 4.0, 4.0]
        model.meta.responses.y2_fi2 = [4.0, 4.1, 4.3, 4.4, 4.5 ,4.6]
        
        mock_surr_y1         = MockSurrogate()
        mock_surr_y1.train_multifi   = mock.Mock()
        model.meta.surrogates['y1'] = mock_surr_y1
        mock_surr_y2         = MockSurrogate()
        mock_surr_y2.train_multifi   = mock.Mock()
        model.meta.surrogates['y2'] = mock_surr_y2

        model.meta.run()
        expected_xtrain=[[[1.0, 1.0], [2.0, 2.0], [3.0, 3.0]], 
                         [[1.1, 2.1], [2.1, 2.2], [3.1, 2.3], 
                          [1.0, 1.0], [2.0, 2.0], [3.0, 3.0]]]
        expected_y1train=[[0.0, 0.1, 0.2], [3.0, 3.1, 3.3, 3.4, 3.5 ,3.6]]      
        expected_y2train=[[4.0, 4.0, 4.0], [4.0, 4.1, 4.3, 4.4, 4.5 ,4.6]]      
        
        mock_surr_y1.train_multifi.assert_called_with(expected_xtrain, expected_y1train)                
        mock_surr_y2.train_multifi.assert_called_with(expected_xtrain, expected_y2train)                

    
    def test_multifidelity_warm_start(self):    
        model = set_as_top(Assembly())
        model.add('meta', MultiFiMetaModel(params=('x',),
                                      responses=('y',), nfi=2))
        model.driver.workflow.add('meta')
        model.meta.warm_restart = True

        model.meta.params.x        = [0.0, 0.4, 1.0]
        model.meta.params.x_fi2    = [0.1, 0.2, 0.3, 0.5, 0.6]
        model.meta.responses.y     = [1.0, 1.4, 2.0]
        model.meta.responses.y_fi2 = [1.1, 1.2, 1.3, 1.5, 1.6]
        
        mock_surr = MockSurrogate()
        mock_surr.train_multifi    = mock.Mock()
        model.meta.surrogates['y'] = mock_surr   
        model.meta.run()
        expected_xtrain=[[[0.0], [0.4], [1.0]], 
                         [[0.1], [0.2], [0.3], [0.5], [0.6]]]
        expected_ytrain=[[1.0, 1.4, 2.0], 
                         [1.1, 1.2, 1.3, 1.5, 1.6]]        
        mock_surr.train_multifi.assert_called_with(expected_xtrain, expected_ytrain)
        
        # Test adding only one lowest fidelity sample
        model.meta.params.x        = []
        model.meta.params.x_fi2    = [2.0]
        model.meta.responses.y     = []
        model.meta.responses.y_fi2 = [1.0]
        
        self.assertTrue(model.meta._train == True)
        model.meta.run()
    
        # Test adding high and low fidelity points
        expected_xtrain=[[[0.0], [0.4], [1.0]], 
                         [[0.1], [0.2], [0.3], [0.5], [0.6], [2.0]]]
        expected_ytrain=[[1.0, 1.4, 2.0], 
                         [1.1, 1.2, 1.3, 1.5, 1.6, 1.0]]        
        mock_surr.train_multifi.assert_called_with(expected_xtrain, expected_ytrain)
        
        model.meta.params.x        = [3.0]
        model.meta.params.x_fi2    = [3.0]
        model.meta.responses.y     = [4.0]
        model.meta.responses.y_fi2 = [4.0]
        self.assertTrue(model.meta._train == True)
        model.meta.run()
    
        expected_xtrain=[[[0.0], [0.4], [1.0], [3.0]], 
                         [[0.1], [0.2], [0.3], [0.5], [0.6], [2.0], [3.0]]]
        expected_ytrain=[[1.0, 1.4, 2.0, 4.0], 
                         [1.1, 1.2, 1.3, 1.5, 1.6, 1.0, 4.0]]        
        mock_surr.train_multifi.assert_called_with(expected_xtrain, expected_ytrain)
        
if __name__ == "__main__":
    unittest.main()


