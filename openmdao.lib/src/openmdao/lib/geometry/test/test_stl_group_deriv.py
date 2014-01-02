"""
Testing differentiation of stl group objects.
"""


import os
import unittest

import numpy as np

from openmdao.lib.components.geomcomp import GeomComponent
from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.interfaces import IParametricGeometry, implements, \
                                     IStaticGeometry
from openmdao.main.variable import Variable
from openmdao.util.testutil import assert_rel_error


from openmdao.lib.geometry.stl_group import STLGroup
import openmdao.lib.geometry.stl as stl
from openmdao.lib.geometry.ffd_axisymetric import Body, Shell
from openmdao.lib.geometry.stl_group import STLGroup

import openmdao.examples.nozzle_geometry_doe


class GeomRecieveDerivApplyDeriv(Component): 
    """Takes an STLGroup object in and outputs an nx3 array of points from that 
    STL Group"""

    geom_in = Variable(iotype='in')
    out = Array(iotype='out')

    def execute(self): 
        self.out = self.geom_in.points


    def linearize(self): 
        
        self.J = np.eye(len(self.geom_in.points))

    def apply_deriv(self, arg, result):
        if 'geom_in' in arg:
            points = arg['geom_in'].reshape(self.geom_in.points.shape)
            result['out'][:,0] += self.J.dot(points[:,0])
            result['out'][:,1] += self.J.dot(points[:,1])
            result['out'][:,2] += self.J.dot(points[:,2])
    
    def apply_derivT(self, arg, result):
        if 'out' in arg:
            result['geom_in'] += self.J.T.dot(arg['out'])

class PlugNozzleGeometry(STLGroup): 

    def __init__(self): 
        super(PlugNozzleGeometry,self).__init__()

        this_dir, this_filename = os.path.split(os.path.abspath(openmdao.examples.nozzle_geometry_doe.__file__))
        plug_file = os.path.join(this_dir, 'plug.stl')
        plug = stl.STL(plug_file)
        cowl_file = os.path.join(this_dir, 'cowl.stl')
        cowl = stl.STL(cowl_file)
        
        n_c = 10
        body = Body(plug,controls=n_c) #just makes n_C evenly spaced points
        body2 = Body(plug.copy(), controls=n_c)
        shell = Shell(cowl,cowl.copy(),n_c,n_c)

        self.add(body,name="plug")
        self.add(body2,name="plug2")
        #self.add(shell,name="cowl")


class TestcaseDerivSTLGroup(unittest.TestCase):

    def setUp(self): 
        self.top = set_as_top(Assembly())
        self.top.add('geom', GeomComponent())
        self.top.geom.add('parametric_geometry', PlugNozzleGeometry())

        self.top.add('rec', GeomRecieveDerivApplyDeriv())

        self.top.connect('geom.geom_out', 'rec.geom_in')

        self.top.driver.workflow.add(['geom','rec'])

        self.top.run()

    def _test_set_array_vals(self): 


        self.top.geom.plug.X = np.array([0,2,0,0,0,0,0,0,0])
        self.top.run()
        p0 = self.top.rec.out.copy()

        self.top.geom.plug.X = np.array([0,0,0,0,0,0,0,0,0]) #reset to 0
        self.top.run()

        self.top.geom.plug.X[1] = 2
        self.top.run()
        p1 = self.top.rec.out.copy()

        self.assertFalse(np.any(p0-p1)) #p0-p1 should be all 0


        self.top.geom.plug.X = np.array([0,0,0,0,0,0,0,0,0]) #reset to 0
        self.top.run()
        self.top.geom.set('plug.X', 2, index=(1,)) 
        self.top.run()
        p2 = self.top.rec.out.copy()

        self.assertFalse(np.any(p0-p2)) #p0-p1 should be all 0



    def test_deriv(self): 

        #self.top.geom.set('plug.X',[0,1,0,0,0,0,0,0,0]) 
        self.top.run()
        self.top.geom.linearize()

        p0 = self.top.rec.out.copy()

        # ins = ['geom.plug.X']
        # outs = ['rec.out']
        # Jx_fd = self.top.driver.workflow.calc_gradient(ins,outs, mode='fd')

        step = 1
        params = ["plug.X","plug2.X"]
        for param in params: 
            offset = self.top.geom.parametric_geometry.param_J_map["plug2.X"]
            for i in xrange(9): 
                tmp = np.array([0,0,0,0,0,0,0,0,0])
                tmp[i] = step
                self.top.geom.set(param,tmp)

                self.top.run()

                p1 = self.top.rec.out.copy()

                FDx = ((p1-p0)/step)[:,0]
                FDy = ((p1-p0)/step)[:,1]
                FDz = ((p1-p0)/step)[:,2]

                Ax = self.top.geom.parametric_geometry.dXqdC[:,offset+i]

                print "%s[%d]"%(param,i), not np.any(np.abs(FDx - Ax) > .00001)

            self.top.geom.set(param, np.array([0,0,0,0,0,0,0,0,0]))
        
    
        self.fail()







if __name__ == "__main__": 

    unittest.main()
