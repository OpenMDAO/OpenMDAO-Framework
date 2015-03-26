import bson
import json
import os.path
import tempfile
import re
import shutil
import sys
import unittest

import h5py
import numpy as np

from struct import unpack
from cStringIO import StringIO

from openmdao.main import __version__
from openmdao.main.api import Assembly, Component, Case, VariableTree, set_as_top
from openmdao.main.datatypes.api import Array, Instance, List, VarTree, Float, Int, Str
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import HDF5CaseRecorder, CaseDataset
from openmdao.lib.drivers.api import SLSQPdriver
from openmdao.lib.drivers.api import FixedPointIterator, SLSQPdriver
from openmdao.lib.optproblems import sellar

from openmdao.examples.simple.paraboloid import Paraboloid

from openmdao.lib.drivers.conmindriver import CONMINdriver

from openmdao.util.testutil import assert_rel_error

from openmdao.lib.drivers.api import SLSQPdriver

from openmdao.lib.drivers.api import SensitivityDriver, CaseIteratorDriver, \
                                     SLSQPdriver
from openmdao.util.testutil import assert_raises


def get_number_of_iterations( hdf5_driver_group ):
    num_iterations = 0
    for key in hdf5_driver_group:
        if key.startswith( "iteration_case_"):
            num_iterations += 1
    return num_iterations



class States(VariableTree):
    y = Array([0.0, 0.0])


class Globals(VariableTree):
    z1 = Float(0.0)
    z2 = Float(0.0)


class Half(Component):
    z2a = Float(0.0, iotype='in')
    z2b = Float(0.0, iotype='out')

    def execute(self):
        self.z2b = 0.5*self.z2a


class SellarMDF(Assembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with FixedPointIterator.
    """

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        # Sub assembly
        sub = self.add('sub', Assembly())

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        sub.add('driver', FixedPointIterator())
        sub.add('dis1', sellar.Discipline1())
        sub.add('dis2', sellar.Discipline2())
        sub.driver.workflow.add(['dis1', 'dis2'])

        # Make all connections
        sub.connect('dis1.y1', 'dis2.y1')
        sub.connect('dis1.z1', 'dis2.z1')

        # Iteration loop
        sub.driver.add_parameter('dis1.y2')
        sub.driver.add_constraint('dis2.y2 = dis1.y2')

        # Solver settings
        sub.driver.max_iteration = 100
        sub.driver.tolerance = .00001
        sub.driver.print_convergence = False

        # Subassy boundaries
        sub.add('globals', VarTree(Globals(), iotype='in'))
        sub.add('states', VarTree(States(), iotype='out'))
        sub.connect('globals.z1', 'dis1.z1')
        # Note, dis1.z2 is connected by input-input conn
        sub.connect('globals.z2', 'dis1.z2')
        sub.connect('globals.z2', 'dis2.z2')
        sub.create_passthrough('dis1.x1')
        sub.connect('dis1.y1', 'states.y[0]')
        sub.connect('dis2.y2', 'states.y[1]')

        # Global Optimization
        self.add('driver', SLSQPdriver())
        self.driver.gradient_options.force_fd = True
        #self.driver.iprint = 3

        # Extra comp
        self.add('half', Half())
        self.connect('half.z2b', 'sub.globals.z2')

        self.driver.workflow.add(['half', 'sub'])

        # Add Parameters to optimizer
        self.driver.add_parameter('sub.globals.z1', low=-10.0, high=10.0)
        self.driver.add_parameter('half.z2a', low=0.0, high=10.0)
        self.driver.add_parameter('sub.x1', low=0.0, high=10.0)

        # Optimization parameters
        self.driver.add_objective('(sub.x1)**2 + sub.globals.z2 + sub.states.y[0] + math.exp(-sub.states.y[1])')

        self.driver.add_constraint('3.16 < sub.states.y[0]')
        self.driver.add_constraint('sub.states.y[1] < 24.0')

        self.sub.globals.z1 = 5.0
        self.half.z2a = 2.0
        self.sub.x1 = 1.0


class TestContainer(VariableTree):

    dummy1 = Float(desc='default value of 0.0') #this value is being grabbed by the optimizer
    dummy2 = Float(11.0)


class TestComponent(Component):

    dummy_data = VarTree(TestContainer(), iotype='in')
    x = Float(iotype='out')

    def execute(self):
        self.x = (self.dummy_data.dummy1-3)**2 - self.dummy_data.dummy2


class TestAssembly(Assembly):

    def configure(self):
        self.add('dummy_top', TestContainer())
        self.add('comp', TestComponent())
        self.add('driver', CONMINdriver())

        self.driver.workflow.add(['comp'])
        #self.driver.iprint = 4 #debug verbosity
        self.driver.add_objective('comp.x')
        self.driver.add_parameter('comp.dummy_data.dummy1', low=-10.0, high=10.0)
 
class TestSellarMDFCase(unittest.TestCase):

    def setUp(self):
        self.tolerance = 0.001
        self.top = set_as_top(SellarMDF())
        self.top.recorders = [ HDF5CaseRecorder('sellar_hdf5.new')]
        self.top.run()

    def tearDown(self):
        self.top = None

    def test_sellarMDF_hdf5_recording(self):

        hdf5_cases_filename = 'sellarMDF.hdf5'
        self.top.recorders = [HDF5CaseRecorder(hdf5_cases_filename)]
        self.top.run()

        ### Check to see if the values written make sense ###
        hdf5_cases_file = h5py.File(hdf5_cases_filename,'r')
        driver_grp = hdf5_cases_file['/iteration_cases/driver/']

        # How many iterations?
        num_iterations = get_number_of_iterations(driver_grp)

        # compare expected to actual last case for some items

        # check an array
        actual = driver_grp['iteration_case_%d/data/array_of_floats' % num_iterations].value
        expected = [  3.18339395e+00,   3.75527804e+00,  -2.02447220e+01,  -8.21709076e-15,  
            -4.10854538e-15,   1.97763916e+00,   3.16000000e+00,   4.94198070e-15,  -3.51305651e-11]
        for exp, act in zip(expected, actual):
            assert_rel_error(self, exp, act, self.tolerance)

        # check a vartree
        actual = driver_grp['iteration_case_%d/data/sub.states/y' % num_iterations].value
        expected = [ 3.16, 3.75527804]
        for exp, act in zip(expected, actual):
            assert_rel_error(self, exp, act, self.tolerance)




class CompWithStringOutput(Component):
    n = Int(0, iotype='in')
    s = Str('', iotype='out')

    def execute(self):
        self.s = 'q' * self.n


class StringOutput(Assembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with FixedPointIterator.
    """

    def configure(self):
        
        comp = self.add('comp', CompWithStringOutput())
        self.driver.workflow.add( 'comp' )



class TestSettingStringLengthCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(StringOutput())

    def tearDown(self):
        self.top = None

    def test_setting_string_length(self):
        hdf5_cases_filename = 'string_output.hdf5'
        self.top.comp.n = 90
        self.top.recorders = [HDF5CaseRecorder(hdf5_cases_filename)]

        # Check to see that an error is thrown if a string is too long
        try:
            self.top.run()
        except ValueError, err:
            self.assertEqual(str(err), "string will not fit in space allocated for in HDF5 file")
        else:
            self.fail("expected ValueError with message: string will not fit in space allocated for in HDF5 file")


if __name__ == '__main__':
    unittest.main()
