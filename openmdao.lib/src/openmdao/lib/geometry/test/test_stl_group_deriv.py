"""
Testing differentiation of stl group objects.
"""


import os
import unittest
import StringIO

import numpy as np

from openmdao.lib.components.geomcomp import GeomComponent
from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.interfaces import IParametricGeometry, implements, \
                                     IStaticGeometry
from openmdao.main.variable import Variable
from openmdao.util.testutil import assert_rel_error


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
        pass

    def apply_deriv(self, arg, result):
        if 'geom_in' in arg:

            result['out'] += arg['geom_in'].reshape(-1,3)
    
    def apply_derivT(self, arg, result):
        if 'out' in arg:
            result['geom_in'] += arg['out'].flatten()

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
        shell = Shell(cowl.copy(),cowl.copy(),n_c,n_c)
        shell2 = Shell(cowl.copy(),cowl.copy(),n_c,n_c)

        self.add(body,name="plug")
        self.add(shell,name="cowl")
        self.add(body2,name="plug2")
        self.add(shell2,name="cowl2")


class TestcaseDerivSTLGroup(unittest.TestCase):

    def setUp(self): 
        self.top = set_as_top(Assembly())
        self.top.add('geom', GeomComponent())
        self.top.geom.add('parametric_geometry', PlugNozzleGeometry())

        self.top.add('rec', GeomRecieveDerivApplyDeriv())

        self.top.connect('geom.geom_out', 'rec.geom_in')

        self.top.driver.workflow.add(['geom','rec'])

        self.top.run()

    def test_set_array_vals(self): 
        self.top.geom.plug.X = np.array([0,2,0,0,0,0,0,0,0])
        self.top.run()
        p0 = self.top.rec.out.copy()

        self.top.geom.plug.X = np.array([0,0,0,0,0,0,0,0,0]) #reset to 0
        self.top.run()
        p1 = self.top.rec.out.copy()
        self.assertTrue(np.any(p0-p1)) #p0-p1 should be nonzero

        self.top.geom.plug.X[1] = 2
        self.top.run()
        p2 = self.top.rec.out.copy()
# lack of invalidation means this won't work
#        self.assertFalse(np.any(p0-p2)) #p0-p2 should be all 0

        self.top.geom.plug.X = np.array([0,0,0,0,0,0,0,0,0]) #reset to 0
        self.top.run()
        p3 = self.top.rec.out.copy()
        self.assertFalse(np.any(p1-p3)) #p1-p3 should be all 0

        self.top.geom.set('plug.X', 2, index=(1,)) 
        self.top.run()
        p4 = self.top.rec.out.copy()
        self.assertFalse(np.any(p0-p4)) #p0-p4 should be all 0

    def test_apply_deriv(self): 

        self.top.run()
        self.top.geom.linearize()

        params = ["plug.X", "plug2.X", "cowl.X", "cowl2.X"]
        for param in params: 
            self.top.driver.workflow.config_changed()
            J = self.top.driver.workflow.calc_gradient(['geom.'+param,],['rec.out',], mode='forward')
            Jx_forward = J[0::3]
            Jy_forward = J[1::3]
            Jz_forward = J[2::3]
            
            self.top.driver.workflow.config_changed()
            J = self.top.driver.workflow.calc_gradient(['geom.'+param,],['rec.out',], mode='adjoint')
            Jx_adjoint = J[0::3]
            Jy_adjoint = J[1::3]
            Jz_adjoint = J[2::3]

            shape = self.top.geom.get(param).shape
            Jx, Jy, Jz = self.top.geom.parametric_geometry.param_J_map[param]

            self.assertTrue(np.any(np.abs(Jx_forward) > 0.0))
            self.assertTrue(np.all(np.abs(Jx - Jx_forward) < .00001))
            self.assertTrue(np.all(np.abs(Jy_forward) < 1e-10))
            self.assertTrue(np.all(np.abs(Jz_forward) < 1e-10))

            self.assertTrue(np.any(np.abs(Jx_adjoint) > 0.0))
            self.assertTrue(np.all(np.abs(Jx - Jx_adjoint) < .00001))
            self.assertTrue(np.all(np.abs(Jy_adjoint) < 1e-10))
            self.assertTrue(np.all(np.abs(Jz_adjoint) < 1e-10))


        params = ["plug.R", "plug2.R", "cowl.R", "cowl2.R"]
        for param in params: 

            self.top.driver.workflow.config_changed()
            J = self.top.driver.workflow.calc_gradient(['geom.'+param,],['rec.out',], mode='forward')
            Jx_forward = J[0::3]
            Jy_forward = J[1::3]
            Jz_forward = J[2::3]

            self.top.driver.workflow.config_changed()
            J = self.top.driver.workflow.calc_gradient(['geom.'+param,],['rec.out',], mode='adjoint')
            Jx_adjoint = J[0::3]
            Jy_adjoint = J[1::3]
            Jz_adjoint = J[2::3]

            shape = self.top.geom.get(param).shape
            #offset = self.top.geom.parametric_geometry.param_J_offset_map[param]
            Jx, Jy, Jz = self.top.geom.parametric_geometry.param_J_map[param]

            self.assertTrue(np.all(np.abs(Jx_forward) < 1e-10))
            self.assertTrue(np.any(np.abs(Jy_forward) > 0.0))
            self.assertTrue(np.any(np.abs(Jz_forward) > 0.0))
            self.assertTrue(np.all(np.abs(Jy - Jy_forward) < .00001))
            self.assertTrue(np.all(np.abs(Jz - Jz_forward) < .00001))

            self.assertTrue(np.all(np.abs(Jx_adjoint) < 1e-10))
            self.assertTrue(np.any(np.abs(Jy_adjoint) > 0.0))
            self.assertTrue(np.any(np.abs(Jz_adjoint) > 0.0))
            self.assertTrue(np.all(np.abs(Jy - Jy_adjoint) < .00001))
            self.assertTrue(np.all(np.abs(Jz - Jz_adjoint) < .00001))


        params = ["cowl.thickness", "cowl2.thickness"]
        for param in params: 
            self.top.driver.workflow.config_changed()
            J = self.top.driver.workflow.calc_gradient(['geom.'+param,],['rec.out',], mode='forward')
            Jx_forward = J[0::3]
            Jy_forward = J[1::3]
            Jz_forward = J[2::3]

            self.top.driver.workflow.config_changed()
            J = self.top.driver.workflow.calc_gradient(['geom.'+param,],['rec.out',], mode='adjoint')
            Jx_adjoint = J[0::3]
            Jy_adjoint = J[1::3]
            Jz_adjoint = J[2::3]


            shape = self.top.geom.get(param).shape
            #offset = self.top.geom.parametric_geometry.param_J_offset_map[param]
            Jx, Jy, Jz = self.top.geom.parametric_geometry.param_J_map[param]

            self.assertTrue(np.all(np.abs(Jx_forward) < 1e-10))
            self.assertTrue(np.any(np.abs(Jy_forward) > 0.0))
            self.assertTrue(np.any(np.abs(Jz_forward) > 0.0))
            self.assertTrue(np.all(np.abs(Jy - Jy_forward) < .00001))
            self.assertTrue(np.all(np.abs(Jz - Jz_forward) < .00001))

            self.assertTrue(np.all(np.abs(Jx_adjoint) < 1e-10))
            self.assertTrue(np.any(np.abs(Jy_adjoint) > 0.0))
            self.assertTrue(np.any(np.abs(Jz_adjoint) > 0.0))
            self.assertTrue(np.all(np.abs(Jy - Jy_adjoint) < .00001))
            self.assertTrue(np.all(np.abs(Jz - Jz_adjoint) < .00001))


    def _test_apply_deriv_fd(self): 

        self.top.run()
        self.top.geom.linearize()

        params = ["plug.X", "plug2.X", "cowl.X", "cowl2.X", "plug.R", "plug2.R", "cowl.R", "cowl2.R", "cowl.thickness", "cowl2.thickness"]
        ins = ['geom.'+param for param in params]
        outs = ['rec.out',]

        self.top.driver.workflow.config_changed()
        J_forward = self.top.driver.workflow.calc_gradient(ins,outs, mode='forward')
        self.top.driver.workflow.config_changed()
        J_fd = self.top.driver.workflow.calc_gradient(ins,outs, mode='fd')

        self.assertTrue(np.any(np.abs(J_forward) > 0.0))
        self.assertTrue(np.any(np.abs(J_fd) > 0.0))
        self.assertTrue(np.all(np.abs(J_forward - J_fd) < .00001))

    def test_jacobian_manual_fd(self): 

        #self.top.geom.set('plug.X',[0,1,0,0,0,0,0,0,0]) 
        self.top.run()
        self.top.geom.linearize()

        p0 = self.top.rec.out.copy()

        # ins = ['geom.plug.X']
        # outs = ['rec.out']
        # Jx_fd = self.top.driver.workflow.calc_gradient(ins,outs, mode='fd')

        step = 1
        params = ["plug.X", "plug2.X", "cowl.X", "cowl2.X"]
        for param in params: 
            
            shape = self.top.geom.get(param).shape
            Jx, Jy, Jz = self.top.geom.parametric_geometry.param_J_map[param]

            for i in xrange(shape[0]): 
                tmp = np.zeros(shape)
                tmp[i] = step
                self.top.geom.set(param,tmp)

                self.top.run()

                p1 = self.top.rec.out.copy()

                FDx = ((p1-p0)/step)[:,0]

                Ax = Jx[:,i]

                #print "%s[%d]"%(param,i), not np.any(np.abs(FDx - Ax) > .00001)
                self.assertTrue(np.all(np.abs(FDx - Ax) < .00001))

            self.top.geom.set(param, np.zeros(shape))

        params = ["plug.R", "plug2.R", "cowl.R", "cowl2.R"]
        for param in params: 
            shape = self.top.geom.get(param).shape
            #offset = self.top.geom.parametric_geometry.param_J_offset_map[param]
            Jx, Jy, Jz = self.top.geom.parametric_geometry.param_J_map[param]
            for i in xrange(shape[0]): 
                tmp = np.zeros(shape)
                tmp[i] = step
                self.top.geom.set(param,tmp)

                self.top.run()

                p1 = self.top.rec.out.copy()

                FDy = ((p1-p0)/step)[:,1]
                FDz = ((p1-p0)/step)[:,2]

                Ay = Jy[:,i]
                Az = Jz[:,i]

                #print "%s[%d]"%(param,i), not np.any(np.abs(FDy - Ay) > .00001), not np.any(np.abs(FDz - Az) > .00001)
                self.assertTrue(np.all(np.abs(FDy - Ay) < .00001))
                self.assertTrue(np.all(np.abs(FDz - Az) < .00001))

            self.top.geom.set(param, np.zeros(shape))

        params = ["cowl.thickness", "cowl2.thickness"]
        for param in params: 
            shape = self.top.geom.get(param).shape
            #offset = self.top.geom.parametric_geometry.param_J_offset_map[param]
            Jx, Jy, Jz = self.top.geom.parametric_geometry.param_J_map[param]
            for i in xrange(shape[0]): 
                tmp = np.zeros(shape)
                tmp[i] = step
                self.top.geom.set(param,tmp)

                self.top.run()

                p1 = self.top.rec.out.copy()

                FDy = ((p1-p0)/step)[:,1]
                FDz = ((p1-p0)/step)[:,2]

                Ay = Jy[:,i]
                Az = Jz[:,i]

                #print "%s[%d]"%(param,i), not np.any(np.abs(FDy - Ay) > .00001), not np.any(np.abs(FDz - Az) > .00001)

                self.assertTrue(np.all(np.abs(FDy - Ay) < .00001))
                self.assertTrue(np.all(np.abs(FDz - Az) < .00001))

            self.top.geom.set(param, np.zeros(shape))


if __name__ == "__main__": 

    unittest.main()
