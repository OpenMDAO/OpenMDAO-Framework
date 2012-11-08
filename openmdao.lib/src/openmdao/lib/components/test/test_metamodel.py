# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import HasTraits

from openmdao.lib.datatypes.api import Float
from openmdao.main.api import Assembly, Component, set_as_top, Case
from openmdao.main.interfaces import implements, ICaseRecorder

from openmdao.main.uncertain_distributions import NormalDistribution

from openmdao.lib.casehandlers.api import ListCaseIterator
from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
from openmdao.lib.surrogatemodels.logistic_regression import LogisticRegression

from openmdao.util.testutil import assert_rel_error


class DumbRecorder(HasTraits):
    implements(ICaseRecorder)
    def record(self,case): 
        pass
    def close(self):
        pass


class Simple(Component):
    
    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 4.
        self.b = 5.
        self.c = 7.
        self.d = 1.5

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b
                
        
class SimpleMatch(Component):
    
    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')
    
    def __init__(self):
        super(SimpleMatch, self).__init__()
        self.a = 4.
        self.b = 5.
        self.c = 7.
        self.d = 1.5

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b
                

class Simple2(Component):
    
    w = Float(iotype='in')
    x = Float(iotype='in')
    y = Float(iotype='out')
    z = Float(iotype='out')
    
    def __init__(self):
        super(Simple2, self).__init__()
        self.w = 4.
        self.x = 5.
        self.y = 7.
        self.z = 1.5

    def execute(self):
        self.y = self.w * 1.1
        self.z = self.x * 0.9
        

class AModel(Component):
    pass


class MyMetaModel(MetaModel):
    my_x = Float(1., iotype='in')
    

class Dummy(Component): 
    x = Float(1.2,iotype="in")
    y = Float(0,iotype="out")

    def execute(self): 
        self.y = 2*self.x
        
class DummyError(Component): 
    x = Float(1.2,iotype="in")
    y = Float(0,iotype="out")

    def execute(self): 
        self.raise_exception("Test Error",RuntimeError)

class Sim(Assembly):
    def configure(self):

        self.add('mm',MetaModel())
        self.mm.default_surrogate = KrigingSurrogate()
        self.mm.model = Dummy()


class MetaModelTestCase(unittest.TestCase):

    def test_model_change(self):
        metamodel = MetaModel()
        mmins = set(metamodel.list_inputs())
        mmouts = set(metamodel.list_outputs())
        metamodel.default_surrogate = KrigingSurrogate()
        metamodel.model = Simple()
        attrs = metamodel.get_attributes(io_only=False)
        for s in attrs['Slots']:
            self.assertNotEqual(s['name'], 'c')
            self.assertNotEqual(s['name'], 'd')
        inputs = set(metamodel.list_inputs())
        outputs = set(metamodel.list_outputs())
        self.assertEquals(inputs-mmins, set(['a','b']))
        self.assertEquals(outputs-mmouts, set(['c','d']))
        for i in range(3):
            metamodel.train_next = True
            metamodel.run()
            
        self.assertTrue(len(metamodel._training_data['c']) == 3)
        self.assertTrue(len(metamodel._training_data['d']) == 3)
        self.assertTrue(len(metamodel._training_input_history) == 3)
        
        metamodel.includes = ['a','b','c','d']
        
        self.assertTrue(len(metamodel._training_data['c']) == 3)
        self.assertTrue(len(metamodel._training_data['d']) == 3)
        self.assertTrue(len(metamodel._training_input_history) == 3)
        
        # removing an output shouldn't clobber the rest of the training data
        metamodel.includes = ['a','b','c']
        
        self.assertTrue(len(metamodel._training_data['c']) == 3)
        self.assertTrue('d' not in metamodel._training_data)
        self.assertTrue(len(metamodel._training_input_history) == 3)
        
        # now put a different model in with the same inputs/outputs
        metamodel.model = SimpleMatch()
        metamodel.includes = ['a','b','c','d']
        inputs = set(metamodel.list_inputs())
        outputs = set(metamodel.list_outputs())
        self.assertEquals(inputs-mmins, set(['a','b']))
        self.assertEquals(outputs-mmouts, set(['c', 'd']))

        self.assertTrue(len(metamodel._training_data['c']) == 0)
        self.assertTrue(len(metamodel._training_data['d']) == 0)
        self.assertTrue(len(metamodel._training_input_history) == 0)

        # now put a different model in
        metamodel.model = Simple2()
        metamodel.includes = ['w','x','y','z']
        inputs = set(metamodel.list_inputs())
        outputs = set(metamodel.list_outputs())
        self.assertEquals(inputs-mmins, set(['w','x']))
        self.assertEquals(outputs-mmouts, set(['y','z']))

    def test_default_surrogate_change(self):
        metamodel = MetaModel()
        mmins = set(metamodel.list_inputs())
        mmouts = set(metamodel.list_outputs())
        metamodel.default_surrogate = KrigingSurrogate()
        metamodel.model = Simple()
        metamodel.default_surrogate = LogisticRegression()
        attrs = metamodel.get_attributes(io_only=False)
        for s in attrs['Slots']:
            self.assertNotEqual(s['name'], 'c')
            self.assertNotEqual(s['name'], 'd')
        inputs = set(metamodel.list_inputs())
        outputs = set(metamodel.list_outputs())
        self.assertEquals(inputs-mmins, set(['a','b']))
        self.assertEquals(outputs-mmouts, set(['c','d']))
        for i in range(3):
            metamodel.train_next = True
            metamodel.run()
            
        self.assertTrue(len(metamodel._training_data['c']) == 3)
        self.assertTrue(len(metamodel._training_data['d']) == 3)
        self.assertTrue(len(metamodel._training_input_history) == 3)
        
        metamodel.includes = ['a','b','c','d']
        
        self.assertTrue(len(metamodel._training_data['c']) == 3)
        self.assertTrue(len(metamodel._training_data['d']) == 3)
        self.assertTrue(len(metamodel._training_input_history) == 3)
        
        # removing an output shouldn't clobber the rest of the training data
        metamodel.includes = ['a','b','c']
        
        self.assertTrue(len(metamodel._training_data['c']) == 3)
        self.assertTrue('d' not in metamodel._training_data)
        self.assertTrue(len(metamodel._training_input_history) == 3)
        
        # now put a different model in with the same inputs/outputs
        metamodel.model = SimpleMatch()
        metamodel.includes = ['a','b','c','d']
        inputs = set(metamodel.list_inputs())
        outputs = set(metamodel.list_outputs())
        self.assertEquals(inputs-mmins, set(['a','b']))
        self.assertEquals(outputs-mmouts, set(['c', 'd']))

        self.assertTrue(len(metamodel._training_data['c']) == 0)
        self.assertTrue(len(metamodel._training_data['d']) == 0)
        self.assertTrue(len(metamodel._training_input_history) == 0)

        # now put a different model in
        metamodel.model = Simple2()
        metamodel.includes = ['w','x','y','z']
        inputs = set(metamodel.list_inputs())
        outputs = set(metamodel.list_outputs())
        self.assertEquals(inputs-mmins, set(['w','x']))
        self.assertEquals(outputs-mmouts, set(['y','z']))    
        
    def _get_assembly(self, default=True, meta=True):
        asm = set_as_top(Assembly())
        asm.add('comp1', Simple())
        asm.add('comp2', Simple())
        if meta:
            asm.add('metamodel', MetaModel())
            
            if default:
                asm.metamodel.default_surrogate = KrigingSurrogate()
            
            asm.metamodel.model = Simple()
            asm.metamodel.recorder = DumbRecorder()
        else:
            asm.add('metamodel', Simple()) # put a real Simple comp in place of metamodel
            
        asm.driver.workflow.add(['metamodel','comp1','comp2'])
        
        asm.connect('comp1.c','metamodel.a')
        asm.connect('comp1.d','metamodel.b')
        asm.connect('metamodel.c','comp2.a')
        asm.connect('metamodel.d','comp2.b')
        return asm
        
    def test_setup1(self):
        meta_asm = self._get_assembly(default=False)
        asm = self._get_assembly(meta=False)
        self.assertTrue('a' in meta_asm.metamodel.list_inputs())
        self.assertTrue('b' in meta_asm.metamodel.list_inputs())
        self.assertEqual(asm.metamodel.a, meta_asm.metamodel.a)
        self.assertEqual(asm.metamodel.b, meta_asm.metamodel.b)
        meta_asm.run()
        asm.run()
        self.assertEqual(asm.metamodel.c, meta_asm.metamodel.c)
        self.assertEqual(asm.metamodel.d, meta_asm.metamodel.d)
        meta_asm.metamodel.excludes = ['a']
        self.assertTrue('a' not in meta_asm.metamodel.list_inputs())
        self.assertTrue('b' in meta_asm.metamodel.list_inputs())
        self.assertTrue(hasattr(meta_asm.metamodel, 'sur_c'))
        self.assertTrue(hasattr(meta_asm.metamodel, 'sur_d'))
        meta_asm.metamodel.excludes = ['d']
        self.assertTrue(hasattr(meta_asm.metamodel, 'sur_c'))
        self.assertTrue(not hasattr(meta_asm.metamodel, 'sur_d'))
        
        
    def test_comp_error(self): 
        a = Assembly()
        a.add('m',MetaModel()) 
        a.m.default_surrogate = KrigingSurrogate()
        a.m.model = DummyError()
        
        a.m.train_next = True
        
        a.driver.workflow.add('m')
        try: 
            a.run()
        except RuntimeError as err: 
            self.assertEqual("m.model: Test Error",str(err))
        else: 
            self.fail("RuntimeError expected")

    
    def test_in_assembly(self):
        asm = self._get_assembly()
        self.assertEqual(set(asm.list_connections()), 
                         set([('metamodel.d', 'comp2.b'), ('metamodel.c', 'comp2.a'), 
                              ('comp1.c', 'metamodel.a'), ('comp1.d', 'metamodel.b')]))
        
        # do some training
        data = [1,2,3,4]
        
        for a,b in zip(data[:-1],data[1:]):
            asm.comp1.a = a
            asm.comp1.b = b
            asm.metamodel.train_next = 1
            asm.run()
            
        # now run and get some results
        asm.comp1.a = 1.
        asm.comp1.b = 2.
        
        asm.run()
        
        assert_rel_error(self, asm.comp2.c, 6, 0.02)
        assert_rel_error(self, asm.comp2.d, -2, 0.02)
        
        asm.metamodel.default_surrogate.nugget = 1
        
        # set new model and verify disconnect
        asm.metamodel.model = Simple2()
        self.assertEqual(asm.list_connections(), [])
        
    def _trained_asm(self, avals, bvals):
        asm = set_as_top(Assembly())
        asm.add('metamodel', MetaModel())
        asm.metamodel.default_surrogate = KrigingSurrogate()
        asm.metamodel.model = Simple()
        asm.metamodel.recorder = DumbRecorder()
        asm.driver.workflow.add(['metamodel'])
        
        for a,b in zip(avals, bvals):
            asm.metamodel.a = a
            asm.metamodel.b = b
            asm.metamodel.train_next = 1
            asm.metamodel.run()
            
        return asm
        
    def test_constant_inputs(self):
        avals = [1.1]*10
        bvals = [2.2]*10
        asm = self._trained_asm(avals, bvals)
                    
        # now run and get some results
        asm.metamodel.a = 1.
        asm.metamodel.b = 2.
        
        try:
            asm.metamodel.run()
        except Exception as err:
            self.assertEqual("metamodel: ERROR: all training inputs are constant.", str(err))
        else:
            self.fail("Exception expected")
            
        asm = self._trained_asm(avals+[1.2], bvals+[2.2])
        asm.metamodel.a = 1.
        asm.metamodel.b = 2.2
        asm.metamodel.run()
        
        # now set b to a value different than the constant training value
        asm.metamodel.b = 4.8
        try:
            asm.metamodel.run()
        except Exception as err:
            self.assertEqual(str(err),
                             "metamodel: ERROR: training input 'b' was a constant value of (2.2) but the value has changed to (4.8).")
        else:
            self.fail("Exception expected")
        
        
    def test_warm_start(self): 
        metamodel = MetaModel()
        metamodel.name = 'meta'
        metamodel.default_surrogate = KrigingSurrogate()
        metamodel.model = Simple()
        metamodel.recorder = DumbRecorder()
        simple = Simple()
        
        cases = []
        
        metamodel.a = 1.
        metamodel.b = 2.
        metamodel.train_next = True
        metamodel.run()
        inputs = [('meta2.a',metamodel.a),('meta2.b',metamodel.b)]
        outputs = [('meta2.c',metamodel.c.mu),('meta2.d',metamodel.d.mu)]
        cases.append(Case(inputs=inputs,outputs=outputs))
        
        metamodel.a = 3.
        metamodel.b = 5.
        metamodel.train_next = True
        metamodel.run()
        inputs = [('meta2.a',metamodel.a),('meta2.b',metamodel.b)]
        outputs = [('meta2.c',metamodel.c.mu),('meta2.d',metamodel.d.mu)]
        cases.append(Case(inputs=inputs,outputs=outputs))
        
        case_iter = ListCaseIterator(cases)
        
        metamodel2 = MetaModel()
        metamodel2.name = 'meta2'
        metamodel2.default_surrogate = KrigingSurrogate()
        metamodel2.model = Simple()
        metamodel2.recorder = DumbRecorder()
        metamodel2.warm_start_data = case_iter
        
        metamodel2.a = simple.a = 1
        metamodel2.b = simple.b = 2
        metamodel.train_next = True
        metamodel2.run()
        simple.run()
        
        self.assertEqual(metamodel2.c.getvalue(), 3.)
        self.assertEqual(metamodel2.d.getvalue(), -1.)
        self.assertEqual(metamodel2.c.getvalue(), simple.c)
        self.assertEqual(metamodel2.d.getvalue(), simple.d)        
        
    def test_default_execute(self):
        metamodel = MetaModel()
        metamodel.name = 'meta'
        metamodel.default_surrogate = KrigingSurrogate()
        metamodel.model = Simple()
        metamodel.recorder = DumbRecorder()
        simple = Simple()
        
        metamodel.a = simple.a = 1.
        metamodel.b = simple.b = 2.
        metamodel.train_next = True
        simple.run()
        metamodel.run()
        
        metamodel.a = simple.a = 1.
        metamodel.b = simple.b = 2.
        metamodel.train_next = True
        simple.run()
        metamodel.run()
        
        self.assertEqual(metamodel.c.getvalue(), 3.)
        self.assertEqual(metamodel.d.getvalue(), -1.)
        self.assertEqual(metamodel.c.getvalue(), simple.c)
        self.assertEqual(metamodel.d.getvalue(), simple.d)
    
    def test_multi_surrogate_models_bad_surrogate_dict(self): 
        metamodel = MetaModel()
        metamodel.name = 'meta'
        metamodel.sur_d = KrigingSurrogate()
        try: 
            metamodel.model = Simple()
        except RuntimeError,err: 
            self.assertEqual("meta: No default surrogate model is defined and the following outputs do not have a surrogate model: ['c']. "
            "Either specify default_surrogate, or specify a surrogate model for all "
            "outputs.",str(err))
        else: 
            self.fail('ValueError expected')
            
        metamodel = MetaModel()
        metamodel.name = 'meta'
        metamodel.sur_d = KrigingSurrogate()
        metamodel.includes = ['a','b','d']
        try: 
            metamodel.model = Simple()
        except ValueError,err: 
            if 'meta: No surrogate was provided for "c". All outputs must have a surrogate'==str(err):
                self.fail('should not get a value error for variable c. It is not included in the metamodel')
        
    def test_multi_surrogate_models(self): 
        metamodel = MetaModel()
        metamodel.name = 'meta'
        metamodel.model = Simple()
        metamodel.sur_d = KrigingSurrogate()
        metamodel.sur_c = LogisticRegression()
        metamodel.recorder = DumbRecorder()
        simple = Simple()
        
        metamodel.a = simple.a = 1.
        metamodel.b = simple.b = 2.
        metamodel.train_next = True
        simple.run()
        metamodel.run()
        
        metamodel.a = simple.a = 3.
        metamodel.b = simple.b = 4.
        metamodel.train_next = True
        simple.run()
        metamodel.run()
        
        self.assertTrue(isinstance(metamodel.d,NormalDistribution))
        self.assertTrue(isinstance(metamodel.c,float))
        
    def test_includes(self):
        metamodel = MyMetaModel()
        metamodel.default_surrogate = KrigingSurrogate()
        metamodel.includes = ['a', 'd']
        metamodel.model = Simple()
        self.assertEqual(metamodel.surrogate_input_names(), ['a'])
        self.assertEqual(metamodel.surrogate_output_names(), ['d'])
        
        # now try changing the includes
        metamodel.includes = ['b', 'c']
        self.assertEqual(metamodel.surrogate_input_names(), ['b'])
        self.assertEqual(metamodel.surrogate_output_names(), ['c'])

    def test_excludes(self):
        metamodel = MyMetaModel()
        metamodel.default_surrogate = KrigingSurrogate()
        metamodel.excludes = ['b', 'd']
        metamodel.model = Simple()
        self.assertEqual(metamodel.surrogate_input_names(), ['a'])
        self.assertEqual(metamodel.surrogate_output_names(), ['c'])
        
        # now try changing the excludes
        metamodel.excludes = ['a', 'c']
        self.assertEqual(metamodel.surrogate_input_names(), ['b'])
        self.assertEqual(metamodel.surrogate_output_names(), ['d'])
        
    def test_include_exclude(self):
        metamodel = MyMetaModel()
        metamodel.default_surrogate = KrigingSurrogate()
        metamodel.includes = ['a','d']
        try:
            metamodel.excludes = ['b','c']
        except RuntimeError as err:
            self.assertEqual(str(err), 
                             ': includes and excludes are mutually exclusive')
        else:
            self.fail('Expected RuntimeError')
        self.assertEqual(metamodel.excludes, [])
            
        metamodel.includes = []
        metamodel.excludes = ['b','c']
        try:
            metamodel.includes = ['a','d']
        except Exception as err:
            self.assertEqual(str(err), 
                             ': includes and excludes are mutually exclusive')
        else:
            self.fail('Expected Exception')
        self.assertEqual(metamodel.includes, [])
        
        
    def test_reset_nochange_inputs(self):
        s = set_as_top(Sim())
        
        s.mm.train_next = True
        s.mm.x = 1
        s.run()
        self.assertEqual(s.mm.x, 1)
        
        s.mm.train_next = True
        s.mm.x = 2
        s.run()
        self.assertEqual(s.mm.x, 2)
        
        s.mm.x = 10
        s.mm.reset_training_data = True
        self.assertEqual(len(s.mm._training_input_history), 0)
        for name in s.mm._training_data:
            self.assertEqual(s.mm._training_data[name],[])

        #all meta model inputs should remain at their current values
        self.assertEqual(s.mm.x, 10)

        
        s.mm.train_next = True
        s.mm.x = 10
        s.run()
        self.assertEqual(s.mm.x, 10)
        
        
        s.mm.train_next = True
        s.mm.x = 20
        s.run()
        self.assertEqual(s.mm.x, 20)
        
        s.mm.train_next = True
        s.mm.x = 30
        s.run()
        self.assertEqual(s.mm.x, 30)
        
        s.run()
        
        
if __name__ == "__main__":
    unittest.main()


