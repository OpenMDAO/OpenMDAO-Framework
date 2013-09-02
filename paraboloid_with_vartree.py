"""
    paraboloid.py - Evaluates the equation (x-3)^2 + xy + (y+4)^2 = 3
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component, VariableTree
from openmdao.lib.datatypes.api import Float, Array, VarTree
import numpy

from openmdao.util.testutil import assert_raises


class VT(VariableTree):

    x = Float(0.0, iotype='in', desc='The variable x')
    #x = Array(numpy.zeros( shape= (2, )), iotype='in', desc='The variable x')

class ParaboloidWithVartree(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    #x = Float(0.0, iotype='in', desc='The variable x')
    vt = VarTree(VT(), iotype='in')
    #x = Array(numpy.zeros( shape= (2, )), iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')        

    def __init__(self):
        super(ParaboloidWithVartree, self).__init__()
        if hasattr( self, '_call_execute' ):
            print "paraboloid_with_array_and_vartree has _call_execute"
        
        
    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """
        
        print "in execute of paraboloid fancy"
        x = self.vt.x
        y = self.y
        
        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0
        
# End paraboloid.py
