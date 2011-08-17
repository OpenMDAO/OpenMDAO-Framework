"""
Test of the derivatives capability.
"""

import unittest

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component, Assembly, ComponentWithDerivatives
from openmdao.lib.datatypes.api import Float, Int
from openmdao.util.testutil import assert_rel_error

class Paraboloid_Derivative(ComponentWithDerivatives):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')

        
    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(Paraboloid_Derivative, self).__init__()

        self.derivatives.declare_second_derivative('f_xy', 'x', 'y')
        self.derivatives.declare_second_derivative('f_xy', 'x', 'x')
        self.derivatives.declare_second_derivative('f_xy', 'y', 'y')
        self.derivatives.declare_first_derivative('f_xy', 'x')
        self.derivatives.declare_first_derivative('f_xy', 'y')
        
        self.ran_real = False

        
    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """
        
        x = self.x
        y = self.y
        
        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

        self.ran_real = True
        
        
    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
        
        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x
    
        self.derivatives.set_first_derivative('f_xy', 'x', df_dx)
        self.derivatives.set_first_derivative('f_xy', 'y', df_dy)
        
    def calculate_second_derivatives(self):
        """Analytical second derivatives"""
        
        df_dxdx = 2.0
        df_dxdy = 1.0
        df_dydy = 2.0
        
        self.derivatives.set_second_derivative('f_xy', 'x', 'x', df_dxdx)
        self.derivatives.set_second_derivative('f_xy', 'x', 'y', df_dxdy)
        self.derivatives.set_second_derivative('f_xy', 'y', 'y', df_dydy)

class SimpleAssembly(Assembly):
    """ Simple assembly"""
    
    def __init__(self):
        """ Initialize it"""
        
        # pylint: disable-msg=E1101
        super(SimpleAssembly, self).__init__()

        self.add('comp1', Paraboloid_Derivative())
        self.driver.workflow.add(['comp1'])
        
class BottomAssembly(Assembly):
    """ Simple assembly"""
    
    def __init__(self):
        """ Initialize it"""
        
        # pylint: disable-msg=E1101
        super(BottomAssembly, self).__init__()

        self.add('comp1', Paraboloid_Derivative())
        self.driver.workflow.add(['comp1'])
        
        self.create_passthrough('comp1.x')
        self.create_passthrough('comp1.y')
        self.create_passthrough('comp1.f_xy')
        
class TopAssembly(Assembly):
    """ Simple assembly"""
    
    def __init__(self):
        """ Initialize it"""
        
        # pylint: disable-msg=E1101
        super(TopAssembly, self).__init__()

        self.add('assy1', BottomAssembly())
        self.driver.workflow.add(['assy1'])
        
class A(ComponentWithDerivatives):
    """ Simple Comp with no Deriv """

    x1 = Float(0.0, iotype='in', desc='The variable x1')
    x2 = Float(0.0, iotype='in', desc='The variable x2')
    y1 = Float(0.0, iotype='out', desc='The variable y1')
    y2 = Float(0.0, iotype='out', desc='The variable y2')

    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(A, self).__init__()

        self.ran_real = False

        
    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """
        
        x1 = self.x1
        x2 = self.x2
        
        self.y1 = 2.0*x1 - 0.5*x2*x2
        self.y2 = 3.0*x1 - 0.2*x2*x2

        self.ran_real = True
        #print self.name
        #print "%f, %f, %f, %f" % (self.x1, self.x2, self.y1, self.y2)

class A_D(A):
    """ Simple Comp with Deriv """

    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(A_D, self).__init__()

        self.derivatives.declare_first_derivative('y1', 'x1')
        self.derivatives.declare_first_derivative('y1', 'x2')
        self.derivatives.declare_first_derivative('y2', 'x1')
        self.derivatives.declare_first_derivative('y2', 'x2')
        self.derivatives.declare_second_derivative('y1', 'x1', 'x1')
        self.derivatives.declare_second_derivative('y1', 'x1', 'x2')
        self.derivatives.declare_second_derivative('y1', 'x2', 'x2')
        self.derivatives.declare_second_derivative('y2', 'x1', 'x1')
        self.derivatives.declare_second_derivative('y2', 'x1', 'x2')
        self.derivatives.declare_second_derivative('y2', 'x2', 'x2')
        
        self.ran_real = False
        
    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
            
        dy1_dx1 = 2.0
        dy2_dx1 = 3.0
        dy1_dx2 = -self.x2
        dy2_dx2 = -0.4*self.x2
    
        self.derivatives.set_first_derivative('y1', 'x1', dy1_dx1)
        self.derivatives.set_first_derivative('y1', 'x2', dy1_dx2)
        self.derivatives.set_first_derivative('y2', 'x1', dy2_dx1)
        self.derivatives.set_first_derivative('y2', 'x2', dy2_dx2)
        
    def calculate_second_derivatives(self):
        """Analytical second derivatives"""
        
        dy1_dx2dx2 = -1.0
        dy2_dx2dx2 = -0.4
        
        self.derivatives.set_second_derivative('y1', 'x2', 'x2', dy1_dx2dx2)
        self.derivatives.set_second_derivative('y2', 'x2', 'x2', dy2_dx2dx2)

class MultiAssy(Assembly):
    """ A reasonably complicated assembly with multiple connections"""
    
    def __init__(self):
        """ Initialize it"""
        
        # pylint: disable-msg=E1101
        super(MultiAssy, self).__init__()

        self.add('A1', A())
        self.add('A2', A_D())
        self.add('A3', A())
        self.add('A4', A_D())
        self.add('A5', A())
        self.driver.workflow.add(['A1', 'A2', 'A3', 'A4', 'A5'])
        
        self.connect('A1.y1','A2.x1')
        self.connect('A1.y2','A3.x2')
        self.connect('A2.y1','A4.x1')
        self.connect('A3.y2','A4.x2')
        self.connect('A4.y1','A5.x1')
        self.connect('A4.y2','A5.x2')
        
        
class DerivativesTestCase(unittest.TestCase):
    """ Test of Component. """

    def setUp(self):
        self.comp = Paraboloid_Derivative()
        self.comp.x = 3
        self.comp.y = 5
        self.comp.run()
        self.comp.ran_real = False
        
    def test_first_derivative(self):

        eps = 1000.0
        self.comp.calc_derivatives(first=True, second=False)

        self.comp.x = 3.0 + eps
        self.comp.run(ffd_order=1)
        fp = self.comp.f_xy
        self.comp.x = 3.0 - eps
        self.comp.run(ffd_order=1)
        fm = self.comp.f_xy
        d_fd_x = (fp-fm)/(2*eps)
    
        self.comp.x = 3
        self.comp.y = 5.0 + eps
        self.comp.run(ffd_order=1)
        fp = self.comp.f_xy
        self.comp.y = 5.0 - eps
        self.comp.run(ffd_order=1)
        fm = self.comp.f_xy
        d_fd_y = (fp-fm)/(2*eps)
        
        self.assertEqual(d_fd_x, 5.0)
        self.assertEqual(d_fd_y, 21.0)
        self.assertEqual(self.comp.ran_real, False)

    def test_second_derivative(self):

        f0 = self.comp.f_xy
        
        eps = 1000.0
        self.comp.calc_derivatives(first=False, second=True)
        
        self.comp.y = 5.0
        self.comp.x = 3.0 + eps
        self.comp.run(ffd_order=2)
        fp = self.comp.f_xy
        self.comp.x = 3.0 - eps
        self.comp.run(ffd_order=2)
        fm = self.comp.f_xy
        d_fd_xx = (fp - 2.0*f0 + fm)/(eps)**2
        
        self.comp.x = 3
        self.comp.y = 5.0 + eps
        self.comp.run(ffd_order=2)
        fp = self.comp.f_xy
        self.comp.y = 5.0 - eps
        self.comp.run(ffd_order=2)
        fm = self.comp.f_xy
        d_fd_yy = (fp - 2.0*f0 + fm)/(eps)**2
        
        self.comp.x = 3.0 + eps
        self.comp.y = 5.0 + eps
        self.comp.run(ffd_order=2)
        fpp = self.comp.f_xy
        self.comp.x = 3.0 + eps
        self.comp.y = 5.0 - eps
        self.comp.run(ffd_order=2)
        fpm = self.comp.f_xy
        self.comp.x = 3.0 - eps
        self.comp.y = 5.0 + eps
        self.comp.run(ffd_order=2)
        fmp = self.comp.f_xy
        self.comp.y = 3.0 - eps
        self.comp.y = 5.0 - eps
        self.comp.run(ffd_order=2)
        fmm = self.comp.f_xy
        d_fd_xy = (fpp - fpm - fmp + fmm)/(2*eps)**2
    
        self.assertEqual(d_fd_xx, 2.0)
        self.assertEqual(d_fd_yy, 2.0)
        self.assertEqual(d_fd_xy, 1.0)
        self.assertEqual(self.comp.ran_real, False)

    def test_bad_variable_declaration(self):
        
        try:
            self.comp.derivatives.declare_first_derivative('x', 'y')
        except RuntimeError, err:
            msg = 'Variable x ' + \
                  'should be an output. '+ \
                  'Derivatives need to be declared for outputs with respect' + \
                  ' to inputs.'
            self.assertEqual(err[0], msg)
        else:
            self.fail('RuntimeError expected')
            
        try:
            self.comp.derivatives.declare_first_derivative('f_xy', 'f_xy')
        except RuntimeError, err:
            msg = 'Variable f_xy ' + \
                  'should be an input. '+ \
                  'Derivatives need to be declared for outputs with respect' + \
                  ' to inputs.'
            self.assertEqual(err[0], msg)
        else:
            self.fail('RuntimeError expected')
            
        try:
            self.comp.derivatives.declare_second_derivative('x', 'y', 'x')
        except RuntimeError, err:
            msg = 'Variable x ' + \
                  'should be an output. '+ \
                  'Derivatives need to be declared for outputs with respect' + \
                  ' to inputs.'
            self.assertEqual(err[0], msg)
        else:
            self.fail('RuntimeError expected')

        self.comp.add('zint', Int(7777, iotype='in'))
        
        try:
            self.comp.derivatives.declare_first_derivative('f_xy', 'zint')
        except RuntimeError, err:
            msg = 'At present, derivatives can only be declared for float-' + \
                  'valued variables. Variable zint ' + \
                  "is of type <type 'str'>."
            self.assertEqual(err[0], msg)
        else:
            self.fail('RuntimeError expected')
            

    def test_forgot_to_declare_first_derivatives(self):
        
        self.comp.add('zx', Float(64.0, iotype='in'))
        self.comp.add('zy', Float(64.0, iotype='in'))
        self.comp.add('zz', Float(64.0, iotype='out'))
        
        try:
            self.comp.derivatives.set_first_derivative('f_xy', 'zx', 33.4)
        except KeyError, err:
            msg = "Derivative of f_xy " + \
                  "with repect to zx " + \
                  "must be declared before being set."            
            self.assertEqual(err[0], msg)
        else:
            self.fail('KeyError expected')

        try:
            self.comp.derivatives.set_first_derivative('zz', 'zx', 33.4)
        except KeyError, err:
            msg = "Derivative of zz " + \
                  "with repect to zx " + \
                  "must be declared before being set."            
            self.assertEqual(err[0], msg)
        else:
            self.fail('KeyError expected')

    def test_forgot_to_declare_second_derivatives(self):
        
        self.comp.add('zx', Float(64.0, iotype='in'))
        self.comp.add('zy', Float(64.0, iotype='in'))
        self.comp.add('zz', Float(64.0, iotype='out'))
        
        try:
            self.comp.derivatives.set_second_derivative('zz', 'zy', 'zy', 33.4)
        except KeyError, err:
            msg = "Derivative of zz " + \
                  "with repect to zy and zy " + \
                  "must be declared before being set."
            self.assertEqual(err[0], msg)
        else:
            self.fail('KeyError expected')

        try:
            self.comp.derivatives.set_second_derivative('f_xy', 'x', 'zx', 33.4)
        except KeyError, err:
            msg = "Derivative of f_xy " + \
                  "with repect to x and zx " + \
                  "must be declared before being set."            
            self.assertEqual(err[0], msg)
        else:
            self.fail('KeyError expected')
            
    def test_unsupported_order(self):
        
        self.comp.calc_derivatives(first=True, second=False)
        try:
            self.comp.derivatives.calculate_output('f_xy', 3)
        except NotImplementedError, err:
            msg = 'Fake Finite Difference does not currently support an ' + \
                  'order of 3.'
            self.assertEqual(err[0], msg)
        else:
            self.fail('NotImplementedError expected')
        
    def test_validate_simple(self):

        # Just making sure it works.
        self.comp.derivatives.validate(1, [], [])
        
    def test_in_assembly(self):
        
        simple = SimpleAssembly()
        simple.comp1.x = 3.0
        simple.comp1.y = 5.0
        simple.run()
        simple.comp1.ran_real = False
        
        eps = 1000.0
        simple.calc_derivatives(first=True, second=False)

        simple.comp1.x = 3.0 + eps
        simple.run(ffd_order=1)
        fp = simple.comp1.f_xy
        simple.comp1.x = 3.0 - eps
        simple.run(ffd_order=1)
        fm = simple.comp1.f_xy
        d_fd_x = (fp-fm)/(2*eps)
    
        simple.comp1.x = 3.0
        simple.comp1.y = 5.0 + eps
        simple.run(ffd_order=1)
        fp = simple.comp1.f_xy
        simple.comp1.y = 5.0 - eps
        simple.run(ffd_order=1)
        fm = simple.comp1.f_xy
        d_fd_y = (fp-fm)/(2.0*eps)
        
        self.assertEqual(d_fd_x, 5.0)
        self.assertEqual(d_fd_y, 21.0)
        self.assertEqual(simple.comp1.ran_real, False)

    def test_in_nested_assembly(self):
        
        simple = TopAssembly()
        simple.assy1.x = 3.0
        simple.assy1.y = 5.0
        simple.run()
        simple.assy1.comp1.ran_real = False
        
        eps = 1000.0
        simple.calc_derivatives(first=True, second=False)

        simple.assy1.x = 3.0 + eps
        simple.run(ffd_order=1)
        fp = simple.assy1.f_xy
        simple.assy1.x = 3.0 - eps
        simple.run(ffd_order=1)
        fm = simple.assy1.f_xy
        d_fd_x = (fp-fm)/(2.0*eps)
    
        simple.assy1.x = 3.0
        simple.assy1.y = 5.0 + eps
        simple.run(ffd_order=1)
        fp = simple.assy1.f_xy
        simple.assy1.y = 5.0 - eps
        simple.run(ffd_order=1)
        fm = simple.assy1.f_xy
        d_fd_y = (fp-fm)/(2.0*eps)
        
        self.assertEqual(d_fd_x, 5.0)
        self.assertEqual(d_fd_y, 21.0)
        self.assertEqual(simple.assy1.comp1.ran_real, False)

    def test_multiblock(self):
        
        comp = MultiAssy()
        comp.A1.x1 = base1 = 1.0
        comp.A1.x2 = base2 = 2.0
        comp.run()
        comp.calc_derivatives(first=True, second=False)
        
        eps = 0.01
        order = 1
        comp.A1.ran_real = False
        comp.A2.ran_real = False
        comp.A3.ran_real = False
        comp.A4.ran_real = False
        comp.A5.ran_real = False
        
        comp.A1.x1 = base1 + eps
        comp.run(ffd_order=order)
        fp = comp.A5.y1
        comp.A1.x1 = base1 - eps
        comp.run(ffd_order=order)
        fm = comp.A5.y1
        dy1_dx1 = (fp-fm)/(2.0*eps)
    
        comp.A1.x1 = base1
        comp.A1.x2 = base2 + eps
        comp.run(ffd_order=order)
        fp = comp.A5.y1
        comp.A1.x2 = base2 - eps
        comp.run(ffd_order=order)
        fm = comp.A5.y1
        dy1_dx2 = (fp-fm)/(2.0*eps)
        
        assert_rel_error(self, dy1_dx1, 12.946, .001)
        assert_rel_error(self, dy1_dx2, -16.835, .001)
        self.assertEqual(comp.A1.ran_real, True)
        self.assertEqual(comp.A2.ran_real, False)
        self.assertEqual(comp.A3.ran_real, True)
        self.assertEqual(comp.A4.ran_real, False)
        self.assertEqual(comp.A5.ran_real, True)
        
if __name__ == '__main__':
    unittest.main()
