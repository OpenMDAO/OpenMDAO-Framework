"""
Testing differentiation of ffd body and shell objects.
"""
import os
import tempfile
import shutil
import unittest

import numpy as np

from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.variable import Variable
from openmdao.util.testutil import assert_rel_error


import openmdao.lib.geometry.stl as stl
from openmdao.lib.geometry.ffd_axisymetric import Body, Shell

import openmdao.examples.nozzle_geometry_doe


class TestFFDDerivatives(unittest.TestCase): 

    def setUp(self): 
        self.this_dir, self.this_filename = os.path.split(os.path.abspath(openmdao.examples.nozzle_geometry_doe.__file__))
        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='test_ffd-')
        os.chdir(self.tempdir)

    def tearDown(self):
        os.chdir(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

    def test_body(self): 
        plug_file = os.path.join(self.this_dir, 'plug.stl')
        plug = stl.STL(plug_file)

        n_c = 5
        body = Body(plug,controls=n_c) #just makes n_C evenly spaced points

        step = 1
        f0 = body.stl.points.copy()

        #x derivatives
        for i in range(n_c): 
            #i = 4
            #deltaC = deltaC_flat.reshape((n_c,2))
            deltaC = np.zeros((n_c,2))
            deltaC[i,0] = step 
            body.deform(deltaC)
            f1 = body.stl.points.copy()

            dfdx = (f1-f0)/step
            deriv_checkX = np.all(np.abs(body.dXqdC[:,i] - dfdx[:,0]) < 1e-6)

            self.assertTrue(deriv_checkX)

        #y,z derivatives
        for i in range(n_c): 
            #i = 4
            #deltaC = deltaC_flat.reshape((n_c,2))
            deltaC = np.zeros((n_c,2))
            deltaC[i,1] = step 
            body.deform(deltaC)
            f1 = body.stl.points.copy()

            dfdx = (f1-f0)/step
            #deriv_checkX = np.all(np.abs(body.dXqdC[:,i] - dfdx[:,0]) < 1e-6)
            deriv_checkY = np.all(np.abs(body.dYqdC[:,i] - dfdx[:,1]) < 1e-6)
            deriv_checkZ = np.all(np.abs(body.dZqdC[:,i] - dfdx[:,2]) < 1e-6)

            #self.assertTrue(deriv_checkX)
            self.assertTrue(deriv_checkY)
            self.assertTrue(deriv_checkZ)


    def test_shell(self): 
        cowl_file = os.path.join(self.this_dir, 'cowl.stl')
        cowl = stl.STL(cowl_file)

        n_c = 5
        step = 1
        shell = Shell(cowl,cowl.copy(),n_c,n_c)

        f0_outer = shell.outer_stl.points.copy()
        f0_inner = shell.inner_stl.points.copy()

        #x derivatives
        for i in range(n_c): 
            deltaCc = np.zeros((n_c,2))
            deltaCt = np.zeros((n_c,2))

            deltaCc[i,0] = step 
            shell.deform(deltaCc, deltaCt)
            f1_outer = shell.outer_stl.points.copy()

            dfdx_outer = (f1_outer-f0_outer)/step
            deriv_checkX_outer = np.all(np.abs(shell.dXoqdCc[:,i] - dfdx_outer[:,0]) < 1e-6)
            #print np.abs(shell.dXoqdCc[:,i] - dfdx_outer[:,0]) 

            self.assertTrue(deriv_checkX_outer)

            f1_inner = shell.inner_stl.points.copy()

            dfdx_inner = (f1_inner-f0_inner)/step
            deriv_checkX_inner = np.all(np.abs(shell.dXiqdCc[:,i] - dfdx_inner[:,0]) < 1e-6)
            #print np.abs(shell.dXiqdCc[:,i] - dfdx_inner[:,0]) 

            self.assertTrue(np.any(shell.dXiqdCc[:,i] > 0.001))
            self.assertTrue(deriv_checkX_inner)

        #y,z derivatives centerline
        for i in range(n_c): 
            deltaCc = np.zeros((n_c,2))
            deltaCt = np.zeros((n_c,2))

            deltaCc[i,1] = step 
            shell.deform(deltaCc, deltaCt)
            f1_outer = shell.outer_stl.points.copy()

            dfdx_outer = (f1_outer-f0_outer)/step
            deriv_checkY_outer = np.all(np.abs(shell.dYoqdCc[:,i] - dfdx_outer[:,1]) < 1e-6)
            deriv_checkZ_outer = np.all(np.abs(shell.dZiqdCc[:,i] - dfdx_outer[:,2]) < 1e-6)
            #print np.abs(shell.dXoqdCc[:,i] - dfdx_outer[:,0]) 

            self.assertTrue(np.any(shell.dYoqdCc[:,i] > 0.001))
            self.assertTrue(deriv_checkY_outer)
            self.assertTrue(np.any(shell.dZoqdCc[:,i] > 0.001))
            self.assertTrue(deriv_checkZ_outer)

            f1_inner = shell.inner_stl.points.copy()

            dfdx_inner = (f1_inner-f0_inner)/step
            deriv_checkY_inner = np.all(np.abs(shell.dYiqdCc[:,i] - dfdx_inner[:,1]) < 1e-6)
            deriv_checkZ_inner = np.all(np.abs(shell.dZiqdCc[:,i] - dfdx_inner[:,2]) < 1e-6)
            #print np.abs(shell.dXiqdCc[:,i] - dfdx_inner[:,0]) 

            self.assertTrue(np.any(shell.dYiqdCc[:,i] > 0.001))
            self.assertTrue(deriv_checkY_inner)
            self.assertTrue(np.any(shell.dZiqdCc[:,i] > 0.001))
            self.assertTrue(deriv_checkZ_inner)

        #y,z derivatives thickness
        for i in range(n_c): 
            deltaCc = np.zeros((n_c,2))
            deltaCt = np.zeros((n_c,2))

            deltaCt[i,1] = step 
            shell.deform(deltaCc, deltaCt)
            f1_outer = shell.outer_stl.points.copy()

            dfdx_outer = (f1_outer-f0_outer)/step
            deriv_checkY_outer = np.all(np.abs(shell.dYoqdCt[:,i] - dfdx_outer[:,1]) < 1e-6)
            deriv_checkZ_outer = np.all(np.abs(shell.dZoqdCt[:,i] - dfdx_outer[:,2]) < 1e-6)
            #print np.abs(shell.dXoqdCc[:,i] - dfdx_outer[:,0]) 

            self.assertTrue(np.any(shell.dYoqdCc[:,i] > 0.001))
            self.assertTrue(deriv_checkY_outer)
            self.assertTrue(np.any(shell.dZoqdCc[:,i] > 0.001))
            self.assertTrue(deriv_checkZ_outer)

            f1_inner = shell.inner_stl.points.copy()

            dfdx_inner = (f1_inner-f0_inner)/step
            deriv_checkY_inner = np.all(np.abs(shell.dYiqdCt[:,i] - dfdx_inner[:,1]) < 1e-6)
            deriv_checkZ_inner = np.all(np.abs(shell.dZiqdCt[:,i] - dfdx_inner[:,2]) < 1e-6)


            self.assertTrue(np.any(shell.dYiqdCt[:,i] > 0.001))
            self.assertTrue(deriv_checkY_inner)
            self.assertTrue(np.any(shell.dZiqdCt[:,i] > 0.001))
            self.assertTrue(deriv_checkZ_inner)

if __name__ == "__main__": 
    unittest.main()
