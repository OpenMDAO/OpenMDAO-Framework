import bson
import json
import os.path
import tempfile
import re
import shutil
import sys
import unittest

from struct import unpack
from cStringIO import StringIO

from openmdao.main import __version__
from openmdao.main.api import Assembly, Component, Case, VariableTree, set_as_top
from openmdao.main.datatypes.api import Array, Instance, List, VarTree, Float
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import HDF5CaseRecorder, CaseDataset
from openmdao.lib.casehandlers.api import JSONCaseRecorder, BSONCaseRecorder, verify_json, CaseDataset

from openmdao.lib.drivers.conmindriver import CONMINdriver



from openmdao.lib.drivers.api import SensitivityDriver, CaseIteratorDriver, \
                                     SLSQPdriver
from openmdao.util.testutil import assert_raises


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

class TestVarTreeCase(unittest.TestCase):

    def test_vartree_hdf5_recording(self):
        blah = set_as_top(TestAssembly())
        blah.recorders = [HDF5CaseRecorder('vartree.hdf5')]
        blah.run()




class TExecComp(ExecComp):
    data = Instance(iotype='in', desc='Used to check bad JSON data')


class Loads(VariableTree):
    Fx = Array()
    Fy = Array()
    Fz = Array()


class LoadsArray(VariableTree):
    loads = List(Loads)


class LoadsComp(Component):
    loads_in  = VarTree(LoadsArray(), iotype='in')
    loads_out = VarTree(LoadsArray(), iotype='out')

    def execute(self):
        self.loads_out = self.loads_in

from openmdao.lib.drivers.api import SLSQPdriver

from openmdao.examples.simple.paraboloid import Paraboloid

class OptimizationUnconstrained(Assembly):
    """Unconstrained optimization of the Paraboloid Component."""
    
    def configure(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
        # pylint: disable-msg=E1101

        # Create Optimizer instance
        self.add('driver', SLSQPdriver())
        
        # Create Paraboloid component instances
        self.add('paraboloid', Paraboloid())

        # Driver process definition
        self.driver.workflow.add('paraboloid')
        
        # SQLSQP Flags
        self.driver.iprint = 0
        
        # Objective 
        self.driver.add_objective('paraboloid.f_xy')
        
        # Design Variables 
        self.driver.add_parameter('paraboloid.x', low=-50., high=50.)
        self.driver.add_parameter('paraboloid.y', low=-50., high=50.)
 
class TestOptimizationCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(OptimizationUnconstrained())

    def tearDown(self):
        self.top = None

    def test_optimization_hdf5_recording(self):

        self.top.recorders = [HDF5CaseRecorder('optimization.hdf5')]
        self.top.run()

class TestCase(unittest.TestCase):

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        driver = top.add('driver', CaseIteratorDriver())
        top.add('comp1', TExecComp(exprs=['z=x+y']))
        top.add('comp2', ExecComp(exprs=['z=x+1']))
        top.connect('comp1.z', 'comp2.x')
        driver.workflow.add(['comp1', 'comp2'])

        # now create some Cases
        outputs = ['comp1.z', 'comp2.z']
        cases = []
        for i in range(10):
            i = float(i)
            inputs = [('comp1.x', i), ('comp1.y', i*2)]
            cases.append(Case(inputs=inputs, outputs=outputs))

        Case.set_vartree_inputs(driver, cases)
        driver.add_responses(outputs)

        self.tempdir = tempfile.mkdtemp(prefix='test_jsonrecorder-')

    def tearDown(self):
        try:
            shutil.rmtree(self.tempdir)
        except OSError:
            pass
        self.top = None

    def test_multiple_objectives(self):
        sout = StringIO()
        self.top.add('driver', SensitivityDriver())
        self.top.driver.workflow.add(['comp1', 'comp2'])
        self.top.driver.add_parameter(['comp1.x'], low=-100, high=100)
        self.top.driver.add_objective('comp1.z')
        self.top.driver.add_objective('comp2.z')

        self.top.recorders = [HDF5CaseRecorder('multiple_objectives.hdf5')]
        self.top.run()

        # with open('multiobj.new', 'w') as out:
        #     out.write(sout.getvalue())
        #verify_json(self, sout, 'multiobj.json')



if __name__ == '__main__':
    unittest.main()
