"""
Testing differentiation of stl group objects.
"""


import os
import tempfile
import shutil
import unittest

import numpy as np

from openmdao.main.api import Component, Assembly, set_as_top, SimulationRoot

import openmdao.lib.geometry.stl as stl
from openmdao.lib.geometry.ffd_axisymetric import Body, Shell
from openmdao.lib.geometry.stl_group import STLGroup

import openmdao.examples.nozzle_geometry_doe


class PlugNozzleGeometry(STLGroup):

    def __init__(self):


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

        geom_parts = (("plug",body),("cowl", shell),("plug2", body2),("cowl2", shell2))

        super(PlugNozzleGeometry,self).__init__(geom_parts=geom_parts)


class TestcaseSTLGroup(unittest.TestCase):

    def setUp(self):

        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='test_stl-')
        os.chdir(self.tempdir)
        SimulationRoot.chroot(self.tempdir)
        
        self.top = set_as_top(Assembly())
        self.top.add('plug_noz', PlugNozzleGeometry())
        self.top.driver.workflow.add('plug_noz')
        self.top.run()

    def tearDown(self):
        os.chdir(self.startdir)
        SimulationRoot.chroot(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

    def test_vars_created(self):
        # make sure the inputs and outputs are created properly

        pn = self.top.plug_noz

        self.assertTrue(hasattr(pn, 'plug'))
        plug = pn.plug
        self.assertTrue(hasattr(plug,'X'))
        self.assertTrue(plug.X.shape, (10,))
        self.assertTrue(hasattr(plug,'R'))
        self.assertTrue(plug.R.shape, (10,))

        self.assertTrue(hasattr(pn, 'plug2'))
        plug2 = pn.plug2
        self.assertTrue(hasattr(plug2,'X'))
        self.assertTrue(plug2.X.shape, (10,))
        self.assertTrue(hasattr(plug2,'R'))
        self.assertTrue(plug2.R.shape, (10,))
        self.assertTrue(hasattr(pn, 'cowl'))
        cowl = pn.cowl
        self.assertTrue(hasattr(cowl,'X'))
        self.assertTrue(cowl.X.shape, (10,))
        self.assertTrue(hasattr(cowl,'R'))
        self.assertTrue(cowl.R.shape, (10,))
        self.assertTrue(hasattr(cowl,'thickness'))
        self.assertTrue(cowl.thickness.shape, (10,))

        self.assertTrue(hasattr(pn, 'cowl2'))
        cowl2 = pn.cowl2
        self.assertTrue(hasattr(cowl2,'X'))
        self.assertTrue(cowl2.X.shape, (10,))
        self.assertTrue(hasattr(cowl2,'R'))
        self.assertTrue(cowl2.R.shape, (10,))
        self.assertTrue(hasattr(cowl2,'thickness'))
        self.assertTrue(cowl2.thickness.shape, (10,))

    def test_set_array_vals(self):
        self.top.plug_noz.plug.X = np.array([0,2,0,0,0,0,0,0,0,0])
        self.top.run()
        p0 = self.top.plug_noz.geom_data.points.copy()

        self.top.plug_noz.plug.X = np.array([0,0,0,0,0,0,0,0,0,0]) #reset to 0
        self.top.run()
        p1 = self.top.plug_noz.geom_data.points.copy()
        self.assertTrue(np.any(p0-p1)) #p0-p1 should be nonzero

        self.top.plug_noz.plug.X[1] = 2
        self.top.run()

        self.top.plug_noz.plug.X = np.array([0,0,0,0,0,0,0,0,0,0]) #reset to 0
        self.top.run()
        p3 = self.top.plug_noz.geom_data.points.copy()
        self.assertFalse(np.any(p1-p3)) #p1-p3 should be all 0

        self.top.plug_noz.set('plug.X[1]', 2)
        self.top.run()
        p4 = self.top.plug_noz.geom_data.points.copy()
        self.assertFalse(np.any(p0-p4)) #p0-p4 should be all 0

    def test_apply_deriv(self):

        self.top.run()
        self.top.plug_noz.provideJ()

        params = ["plug.X", "plug2.X", "cowl.X", "cowl2.X"]
        for param in params:
            J = self.top.driver.calc_gradient(['plug_noz.'+param,],['plug_noz.geom_data.points',], mode='forward')
            Jx_forward = J[0::3]
            Jy_forward = J[1::3]
            Jz_forward = J[2::3]

            J = self.top.driver.calc_gradient(['plug_noz.'+param,],['plug_noz.geom_data.points',], mode='adjoint')
            Jx_adjoint = J[0::3]
            Jy_adjoint = J[1::3]
            Jz_adjoint = J[2::3]

            shape = self.top.plug_noz.get(param).shape
            Jx, Jy, Jz = self.top.plug_noz.param_J_map[param]

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

            J = self.top.driver.calc_gradient(['plug_noz.'+param,],['plug_noz.geom_data.points',], mode='forward')
            Jx_forward = J[0::3]
            Jy_forward = J[1::3]
            Jz_forward = J[2::3]

            J = self.top.driver.calc_gradient(['plug_noz.'+param,],['plug_noz.geom_data.points',], mode='adjoint')
            Jx_adjoint = J[0::3]
            Jy_adjoint = J[1::3]
            Jz_adjoint = J[2::3]

            shape = self.top.plug_noz.get(param).shape
            #offset = self.top.geom.parametric_geometry.param_J_offset_map[param]
            Jx, Jy, Jz = self.top.plug_noz.param_J_map[param]

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
            J = self.top.driver.calc_gradient(['plug_noz.'+param,],['plug_noz.geom_data.points',], mode='forward')
            Jx_forward = J[0::3]
            Jy_forward = J[1::3]
            Jz_forward = J[2::3]

            J = self.top.driver.calc_gradient(['plug_noz.'+param,],['plug_noz.geom_data.points',], mode='adjoint')
            Jx_adjoint = J[0::3]
            Jy_adjoint = J[1::3]
            Jz_adjoint = J[2::3]


            shape = self.top.plug_noz.get(param).shape
            #offset = self.top.geom.parametric_geometry.param_J_offset_map[param]
            Jx, Jy, Jz = self.top.plug_noz.param_J_map[param]

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


    def test_apply_deriv_fd(self):

        self.top.run()
        self.top.plug_noz.provideJ()

        params = ["plug.X", "plug2.X", "cowl.X", "cowl2.X", "plug.R", "plug2.R", "cowl.R", "cowl2.R", "cowl.thickness", "cowl2.thickness"]
        ins = ['plug_noz.'+param for param in params]
        outs = ['plug_noz.geom_data.points',]

        J_forward = self.top.driver.calc_gradient(ins,outs, mode='forward')
        J_fd = self.top.driver.calc_gradient(ins,outs, mode='fd')

        self.assertTrue(np.any(np.abs(J_forward) > 0.0))
        self.assertTrue(np.any(np.abs(J_fd) > 0.0))
        self.assertTrue(np.all(np.abs(J_forward - J_fd) < .00001))

    def test_jacobian_manual_fd(self):

        self.top.run()
        self.top.plug_noz.provideJ()

        p0 = self.top.plug_noz.geom_data.points.copy()

        step = 1
        params = ["plug.X", "plug2.X", "cowl.X", "cowl2.X"]
        for param in params:

            shape = self.top.plug_noz.get(param).shape
            Jx, Jy, Jz = self.top.plug_noz.param_J_map[param]

            for i in xrange(shape[0]):
                tmp = np.zeros(shape)
                tmp[i] = step
                self.top.plug_noz.set(param,tmp)

                self.top.run()

                p1 = self.top.plug_noz.geom_data.points.copy()

                FDx = ((p1-p0)/step)[:,0]

                Ax = Jx[:,i]

                #print "%s[%d]"%(param,i), not np.any(np.abs(FDx - Ax) > .00001)
                self.assertTrue(np.all(np.abs(FDx - Ax) < .00001))

            self.top.plug_noz.set(param, np.zeros(shape))

        params = ["plug.R", "plug2.R", "cowl.R", "cowl2.R"]
        for param in params:
            shape = self.top.plug_noz.get(param).shape
            #offset = self.top.geom.parametric_geometry.param_J_offset_map[param]
            Jx, Jy, Jz = self.top.plug_noz.param_J_map[param]
            for i in xrange(shape[0]):
                tmp = np.zeros(shape)
                tmp[i] = step
                self.top.plug_noz.set(param,tmp)

                self.top.run()

                p1 = self.top.plug_noz.geom_data.points.copy()

                FDy = ((p1-p0)/step)[:,1]
                FDz = ((p1-p0)/step)[:,2]

                Ay = Jy[:,i]
                Az = Jz[:,i]

                #print "%s[%d]"%(param,i), not np.any(np.abs(FDy - Ay) > .00001), not np.any(np.abs(FDz - Az) > .00001)
                self.assertTrue(np.all(np.abs(FDy - Ay) < .00001))
                self.assertTrue(np.all(np.abs(FDz - Az) < .00001))

            self.top.plug_noz.set(param, np.zeros(shape))

        params = ["cowl.thickness", "cowl2.thickness"]
        for param in params:
            shape = self.top.plug_noz.get(param).shape
            #offset = self.top.geom.parametric_geometry.param_J_offset_map[param]
            Jx, Jy, Jz = self.top.plug_noz.param_J_map[param]
            for i in xrange(shape[0]):
                tmp = np.zeros(shape)
                tmp[i] = step
                self.top.plug_noz.set(param,tmp)

                self.top.run()

                p1 = self.top.plug_noz.geom_data.points.copy()

                FDy = ((p1-p0)/step)[:,1]
                FDz = ((p1-p0)/step)[:,2]

                Ay = Jy[:,i]
                Az = Jz[:,i]

                #print "%s[%d]"%(param,i), not np.any(np.abs(FDy - Ay) > .00001), not np.any(np.abs(FDz - Az) > .00001)

                self.assertTrue(np.all(np.abs(FDy - Ay) < .00001))
                self.assertTrue(np.all(np.abs(FDz - Az) < .00001))

            self.top.plug_noz.set(param, np.zeros(shape))


if __name__ == "__main__":

    unittest.main()
