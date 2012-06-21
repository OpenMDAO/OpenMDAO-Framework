"""
Test of the finite difference helper object
"""

import unittest

from openmdao.lib.differentiators.fd_helper import FDhelper
from openmdao.main.api import Assembly, set_as_top
from openmdao.test.execcomp import ExecComp, ExecCompWithDerivatives
from openmdao.util.testutil import assert_rel_error

class FDHelperTestCase(unittest.TestCase):
    """ Test of the finite difference helper object. """

    def test_large_dataflow(self):
        
        self.top = set_as_top(Assembly())
    
        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']
    
        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']
        
        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']
    
        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']
        
        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']
        
        #self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        #self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        #self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        #self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        #self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))
    
        self.top.add('comp1', ExecComp(exp1))
        self.top.add('comp2', ExecComp(exp2))
        self.top.add('comp3', ExecComp(exp3))
        self.top.add('comp4', ExecComp(exp4))
        self.top.add('comp5', ExecComp(exp5))
    
        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')
    
        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', \
                                      'comp4', 'comp5'])
        self.top.comp1.x1 = 2.0
        self.top.run()
        
        comps = ['comp2', 'comp3', 'comp4']
        wrt = ['comp2.x1', 'comp3.x1']
        outs = ['comp4.y1', 'comp4.y2', 'comp4.y3']
        fd = FDhelper(self.top, comps, wrt, outs)
        
        input_dict = {}
        for item in wrt:
            input_dict[item] = self.top.get(item)
            
        output_dict = {}
        for item in outs:
            output_dict[item] = self.top.get(item)
                
        derivs = fd.run(input_dict, output_dict)
        
        assert_rel_error(self, derivs['comp2.x1']['comp4.y1'], 0.5, .001)
        assert_rel_error(self, derivs['comp2.x1']['comp4.y2'], 1.5, .001)
        assert_rel_error(self, derivs['comp2.x1']['comp4.y3'], 10.5, .001)
        assert_rel_error(self, derivs['comp3.x1']['comp4.y1'], 7.0, .001)
        assert_rel_error(self, derivs['comp3.x1']['comp4.y2'], 0.0, .001)
        assert_rel_error(self, derivs['comp3.x1']['comp4.y3'], 14.0, .001)

        fd.model.driver.distribution_generator.form = 'FORWARD'
        derivs = fd.run(input_dict, output_dict)
        
        assert_rel_error(self, derivs['comp2.x1']['comp4.y1'], 0.5, .001)
        assert_rel_error(self, derivs['comp2.x1']['comp4.y2'], 1.5, .001)
        assert_rel_error(self, derivs['comp2.x1']['comp4.y3'], 10.5, .001)
        assert_rel_error(self, derivs['comp3.x1']['comp4.y1'], 7.0, .001)
        assert_rel_error(self, derivs['comp3.x1']['comp4.y2'], 0.0, .001)
        assert_rel_error(self, derivs['comp3.x1']['comp4.y3'], 14.0, .001)

        fd.model.driver.distribution_generator.form = 'BACKWARD'
        derivs = fd.run(input_dict, output_dict)
        
        assert_rel_error(self, derivs['comp2.x1']['comp4.y1'], 0.5, .001)
        assert_rel_error(self, derivs['comp2.x1']['comp4.y2'], 1.5, .001)
        assert_rel_error(self, derivs['comp2.x1']['comp4.y3'], 10.5, .001)
        assert_rel_error(self, derivs['comp3.x1']['comp4.y1'], 7.0, .001)
        assert_rel_error(self, derivs['comp3.x1']['comp4.y2'], 0.0, .001)
        assert_rel_error(self, derivs['comp3.x1']['comp4.y3'], 14.0, .001)

if __name__ == "__main__":
    unittest.main()
