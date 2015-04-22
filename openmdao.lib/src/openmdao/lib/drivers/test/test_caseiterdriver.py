"""
Test CaseIteratorDriver.
"""

import logging
import os
import pkg_resources
import re
import subprocess
import sys
import time
import tempfile
import shutil
import unittest
import nose
from nose import SkipTest

import random
import numpy.random as numpy_random

from math import isnan
from numpy import asarray, linspace, mean

from openmdao.main.api import Assembly, Component, VariableTree, set_as_top, \
                              SimulationRoot
from openmdao.main.eggchecker import check_save_load

from openmdao.main.datatypes.api import Float, Bool, Array, Int, Str, \
                                        List, VarTree
from openmdao.lib.casehandlers.api import ListCaseRecorder
from openmdao.lib.drivers.api import CaseIteratorDriver, SimpleCaseIterDriver, \
                                     SLSQPdriver

from openmdao.main.case import Case, CaseTreeNode

from openmdao.test.cluster import init_cluster

from openmdao.util.testutil import assert_raises, assert_rel_error

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()


def replace_uuid(msg):
    """ Replace UUID in `msg` with ``UUID``. """
    pattern = '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}'
    return re.sub(pattern, 'UUID', msg)


def rosen_suzuki(x):
    """ Evaluate polynomial from CONMIN manual. """
    return x[0]**2 - 5.*x[0] + x[1]**2 - 5.*x[1] + \
           2.*x[2]**2 - 21.*x[2] + x[3]**2 + 7.*x[3] + 50


class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    x = Array([1., 1., 1., 1.], iotype='in')
    y = Array([1., 1., 1., 1., 1., 1., 1., 1., 1., 1.], iotype='in')
    raise_error = Bool(False, iotype='in')
    sleep = Float(0., iotype='in')

    rosen_suzuki = Float(0., iotype='out')
    sum_y = Float(0., iotype='out')

    def __init__(self):
        super(DrivenComponent, self).__init__()

    def execute(self):
        """ Compute results from input vector. """
        self._logger.critical('execute x %s, y %s, raise_error %s',
                              self.x, self.y, self.raise_error)
        if self.sleep:
            time.sleep(self.sleep)
        self.rosen_suzuki = rosen_suzuki(self.x)
        self.sum_y = sum(self.y)
        if self.raise_error:
            self.raise_exception('Forced error', RuntimeError)


class MyModel(Assembly):
    """ Use CaseIteratorDriver with DrivenComponent. """

    def configure(self):
        driver = self.add('driver', CaseIteratorDriver())
        self.add('driven', DrivenComponent())
        driver.workflow.add('driven')
        driver.add_parameter('driven.x')
        driver.add_parameter('driven.y')
        driver.add_parameter('driven.raise_error')
        driver.add_response('driven.rosen_suzuki')
        driver.add_response('driven.sum_y')


class Generator(Component):
    """ Generates cases to be evaluated. """

    x = List(iotype='out')
    y = List(iotype='out')

    def execute(self):
        """ Generate some cases to be evaluated. """
        self.x = [numpy_random.normal(size=4) for i in range(10)]
        self.y = [numpy_random.normal(size=10) for i in range(10)]


class Verifier(Component):
    """ Verifies evaluated cases. """

    x = List(iotype='in')
    y = List(iotype='in')
    rosen_suzuki = List(iotype='in')
    sum_y = List(iotype='in')

    def execute(self):
        """ Verify evaluated cases. """
        for i in range(len(self.rosen_suzuki)):
            assert self.rosen_suzuki[i] == rosen_suzuki(self.x[i])
            assert self.sum_y[i] == sum(self.y[i])


class TracedComponent(Component):
    """ Used to check iteration coordinates. """

    inp = Int(iotype='in')
    itername = Str(iotype='out')

    def execute(self):
        """ Record iteration coordinate. """
        print self.get_pathname(), self.get_itername()
        self.itername = self.get_itername()


class CIDriver(CaseIteratorDriver):

    def __init__(self, max_iterations, comp_name):
        super(CIDriver, self).__init__()
        self.max_iterations = max_iterations
        self.comp_name = comp_name

    def setup_init(self):
        super(CIDriver, self).setup_init()
        inp = self.comp_name+'.x'
        out = self.comp_name+'.y'
        cases = []
        for i in range(self.max_iterations):
            cases.append(Case(inputs=[(inp, i)], outputs=[out]))
        Case.set_vartree_inputs(self, cases)


class CaseComponent(Component):

    x = Float(iotype='in')
    y = Float(iotype='out')

    def execute(self):
        self.y = self.x


class TreeModel(Assembly):

    def configure(self):
        self.recorders = [ListCaseRecorder()]

        self.add('driver2', CIDriver(3, 'comp2'))
        self.add('comp2', CaseComponent())
        self.driver2.workflow.add('comp2')

        self.add('driver1', CIDriver(2, 'comp1'))
        self.add('comp1', CaseComponent())
        self.driver1.workflow.add(['comp1', 'driver2'])

        self.driver.workflow.add('driver1')


class TestCase(unittest.TestCase):
    """ Test CaseIteratorDriver. """

    # Need to be in this directory or there are issues with egg loading.
    directory = pkg_resources.resource_filename('openmdao.lib.drivers', 'test')

    def setUp(self):
        random.seed(10)
        numpy_random.seed(10)

        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='test_caseiter-')
        os.chdir(self.tempdir)
        SimulationRoot.chroot(self.tempdir)

        #os.chdir(self.directory)
        self.model = set_as_top(MyModel())
        self.generate_cases()

    def generate_cases(self, force_errors=False):
        driver = self.model.driver
        driver.case_inputs.driven.x = \
            [numpy_random.normal(size=4) for i in range(10)]
        driver.case_inputs.driven.y = \
            [numpy_random.normal(size=10) for i in range(10)]
        driver.case_inputs.driven.raise_error = \
            [force_errors and i % 4 == 3 for i in range(10)]

    def tearDown(self):
        self.model.pre_delete()
        self.model = None

        os.chdir(self.startdir)
        SimulationRoot.chroot(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

    def test_sequential(self):
        logging.debug('')
        logging.debug('test_sequential')
        self.run_cases(sequential=True)

    def test_sequential_errors(self):
        logging.debug('')
        logging.debug('test_sequential_errors')
        self.generate_cases(force_errors=True)
        self.run_cases(sequential=True, forced_errors=True, retry=False)
        self.run_cases(sequential=True, forced_errors=True, retry=True)

    def test_concurrent(self):
        # This can always test using a LocalAllocator (forked processes).
        logging.debug('')
        logging.debug('test_concurrent')
        init_cluster(encrypted=True, allow_shell=True)
        self.run_cases(sequential=False)

    def test_concurrent_errors(self):
        logging.debug('')
        logging.debug('test_concurrent_errors')
        init_cluster(encrypted=True, allow_shell=True)
        self.generate_cases(force_errors=True)
        self.run_cases(sequential=False, forced_errors=True, retry=False)
        self.run_cases(sequential=False, forced_errors=True, retry=True)

    def test_unencrypted(self):
        logging.debug('')
        logging.debug('test_unencrypted')
        name = init_cluster(encrypted=False, allow_shell=True)
        self.model.driver.extra_resources = {'allocator': name}
        self.run_cases(sequential=False)

    def run_cases(self, sequential, forced_errors=False, retry=True):
        """ Evaluate cases, either sequentially or across multiple servers. """
        driver = self.model.driver
        driver.sequential = sequential
        if not sequential:
            # Try to ensure more than one worker is used.
            self.model.driven.sleep = 0.2
        driver.error_policy = 'RETRY' if retry else 'ABORT'

        if retry:
            self.model.run()
            self.assertEqual(len(driver.case_outputs.driven.rosen_suzuki),
                             len(driver.case_inputs.driven.x))
            self.assertEqual(len(driver.case_outputs.driven.sum_y),
                             len(driver.case_inputs.driven.y))
            self.verify_results(forced_errors)
        else:
            try:
                self.model.run()
            except Exception as err:
                err = replace_uuid(str(err))
                if not sequential:  # RemoteError has different format.
                    err = err.strip().strip('-').strip()
                startmsg = 'driver: Run aborted: Traceback '
                endmsg = 'driven (4-driven): Forced error'
                self.assertEqual(err[:len(startmsg)], startmsg)
                self.assertEqual(err[-len(endmsg):], endmsg)
            else:
                self.fail("Exception expected")

    def verify_results(self, forced_errors=False):
        """ Verify recorded results match expectations. """
        driver = self.model.driver
        for i in range(len(driver.case_inputs.driven.x)):
            error_expected = forced_errors and i % 4 == 3
            if error_expected:
                rs = driver.case_outputs.driven.rosen_suzuki[i]
                sy = driver.case_outputs.driven.sum_y[i]
                print rs, type(rs), sy, type(sy)
                self.assertTrue(isnan(driver.case_outputs.driven.rosen_suzuki[i]))
                self.assertTrue(isnan(driver.case_outputs.driven.sum_y[i]))
            else:
                self.assertEqual(driver.case_outputs.driven.rosen_suzuki[i],
                                 rosen_suzuki(driver.case_inputs.driven.x[i]))
                self.assertEqual(driver.case_outputs.driven.sum_y[i],
                                 sum(driver.case_inputs.driven.y[i]))

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        # Set local dir in case we're running in a different directory.
        py_dir = self.directory

        # Exercise check_save_load().
        retcode = check_save_load(self.model, py_dir=py_dir)
        self.assertEqual(retcode, 0)

    def test_noresource(self):
        logging.debug('')
        logging.debug('test_noresource')

        # Check response to unsupported resource.
        self.model.driver.extra_resources = {'allocator': 'LocalHost',
                                             'localhost': False}
        self.model.driver.sequential = False
        assert_raises(self, 'self.model.run()', globals(), locals(),
                      RuntimeError,
                      'driver: No servers supporting required resources')

    def test_connections(self):
        logging.debug('')
        logging.debug('test_connections')

        top = Assembly()
        top.add('generator', Generator())
        cid = top.add('cid', CaseIteratorDriver())
        top.add('driven', DrivenComponent())
        top.add('verifier', Verifier())

        top.driver.workflow.add(('generator', 'cid', 'verifier'))
        cid.workflow.add('driven')
        cid.add_parameter('driven.x')
        cid.add_parameter('driven.y')
        cid.add_response('driven.rosen_suzuki')
        cid.add_response('driven.sum_y')

        top.connect('generator.x', 'cid.case_inputs.driven.x')
        top.connect('generator.y', 'cid.case_inputs.driven.y')

        top.connect('generator.x', 'verifier.x')
        top.connect('generator.y', 'verifier.y')
        top.connect('cid.case_outputs.driven.rosen_suzuki', 'verifier.rosen_suzuki')
        top.connect('cid.case_outputs.driven.sum_y', 'verifier.sum_y')

        top.run()

    def test_itername(self):
        logging.debug('')
        logging.debug('test_itername')

        top = set_as_top(Assembly())
        cid = top.add('driver', CaseIteratorDriver())
        top.add('comp1', TracedComponent())
        top.add('comp2', TracedComponent())
        cid.workflow.add(('comp1', 'comp2'))

        cid.add_parameter('comp1.inp')
        cid.add_parameter('comp2.inp')
        cid.add_response('comp1.itername')
        cid.add_response('comp2.itername')

        # Sequential.
        cid.case_inputs.comp1.inp = range(3)
        cid.case_inputs.comp2.inp = range(3)
        top.run()
        self.verify_itername(cid)

        # Concurrent.
        top.driver.sequential = False
        cid.case_inputs.comp1.inp = range(3)
        cid.case_inputs.comp2.inp = range(3)
        top.run()
        self.verify_itername(cid)

    def verify_itername(self, cid, subassembly=False):
        # These iternames will have the case's uuid prepended.
        expected = (('1-comp1', '1-comp2'),
                    ('2-comp1', '2-comp2'),
                    ('3-comp1', '3-comp2'))

        outs = cid.case_outputs
        for i in range(3):
            logging.debug('%s: %r %r', i,
                          outs.comp1.itername, outs.comp2.itername)
            if subassembly:
                prefix = '1-sub'
                prefix1, _, iter1 = outs.comp1.itername[i].partition('.')
                prefix2, _, iter2 = outs.comp2.itername[i].partition('.')
                self.assertEqual(prefix1, prefix)
                self.assertEqual(prefix2, prefix)
            else:
                iter1 = outs.comp1.itername[i]
                iter2 = outs.comp2.itername[i]

            self.assertEqual(iter1, expected[i][0])
            self.assertEqual(iter2, expected[i][1])

    def test_subassembly(self):
        logging.debug('')
        logging.debug('test_subassembly')

        top = set_as_top(Assembly())
        sub = top.add('sub', Assembly())
        top.driver.workflow.add('sub')

        cid = sub.add('driver', CaseIteratorDriver())
        sub.add('comp1', TracedComponent())
        sub.add('comp2', TracedComponent())
        cid.workflow.add(('comp1', 'comp2'))

        cid.add_parameter('comp1.inp')
        cid.add_parameter('comp2.inp')
        cid.add_response('comp1.itername')
        cid.add_response('comp2.itername')

        # Sequential.
        cid.case_inputs.comp1.inp = range(3)
        cid.case_inputs.comp2.inp = range(3)
        top.run()
        self.verify_itername(cid, subassembly=True)

        # Concurrent.
        sub.driver.sequential = False
        cid.case_inputs.comp1.inp = range(3)
        cid.case_inputs.comp2.inp = range(3)
        top.run()
        self.verify_itername(cid, subassembly=True)

    def test_main_module_slot(self):
        logging.debug('')
        logging.debug('test_main_module_slot')

        orig_dir = os.getcwd()
        os.chdir(pkg_resources.resource_filename('openmdao.lib.drivers', 'test'))
        try:
            cmdline = [sys.executable, 'cid_slot.py']
            stdout = open('cid_slot.out', 'w')
            retcode = subprocess.call(cmdline, stdout=stdout,
                                      stderr=subprocess.STDOUT)
            stdout.close()
            stdout = open('cid_slot.out', 'r')
            for line in stdout:
                logging.debug('    %s', line.rstrip())
            stdout.close()
            os.remove('cid_slot.out')
        finally:
            os.chdir(orig_dir)

        self.assertEqual(retcode, 0)

    def test_casetree(self):
        # Record tree of cases via CaseIteratorDriver.
        top = set_as_top(TreeModel())
        top.driver1.sequential = True
        top.driver2.sequential = True
        top.run()
        expected = [
            '1',
            '1-driver1.1',
            '1-driver1.1-driver2.1',
            '1-driver1.1-driver2.2',
            '1-driver1.1-driver2.3',
            '1-driver1.2',
            '1-driver1.2-driver2.1',
            '1-driver1.2-driver2.2',
            '1-driver1.2-driver2.3'
        ]
        self.verify_tree(top, expected)


    def test_casetree_concurrent(self):
        raise SkipTest("There are issues with conncurrent CIDs and CID is going to be re-written, so just skip this test for now")
        # Nested CaseIteratorDrivers have some issues:
        # 1. If the second level is concurrent, the first level's iterator
        #    can't be pickled.
        # 2. If the first level is concurrent, we don't see the second level's
        #    recorded cases (they're remote).
        top = set_as_top(TreeModel())
        top.driver1.sequential = False
        top.driver2.sequential = True
        top.run()
        expected = [
            '1',
            '1-driver1.1',
            '1-driver1.2'
        ]
        self.verify_tree(top, expected)

    def verify_tree(self, top, expected):
        print
        print 'Forest:'
        roots = CaseTreeNode.sort(top.recorders[0].get_iterator())
        for root in roots:
            root.dump(1)

        print
        print 'Iternames:'
        for root in roots:
            for name in root.iternames():
                print '   ', name

        for i, name in enumerate(roots[0].iternames()):
            self.assertEqual(name, expected[i])

    def test_noflat(self):

        class A(Component):
            x = Float(iotype='in', noflat=True)
            X = Array(iotype='out', noflat=True)

        class Analysis(Assembly):
            def configure(self):
                self.add('a', A())
                self.add('driver', CaseIteratorDriver())
                self.driver.add_parameter('a.x')
                self.driver.add_response('a.X')

                self.driver.workflow.add('a')

        top = set_as_top(Analysis())
        top.run()
        self.assertTrue(top._pseudo_0._meta['out0'].get('noflat') == True)


# Test bugs reported by Pierre-Elouan Rethore regarding problems using List.

class C0_l(Component):
    l = List([], iotype='out')
    N = Int(10, iotype='in')

    def execute(self):
        self.l = range(self.N)


class C1_l(Component):
    l = List([], iotype='in')
    i = Int(0, iotype='in')
    val = Int(0, iotype='out')

    def execute(self):
        self.val = self.l[self.i]


class A_l(Assembly):
    def configure(self):
        self.add('c0', C0_l())
        self.add('c1', C1_l())

        cid = self.add('parallel_driver', CaseIteratorDriver())
        self.driver.workflow.add(['c0', 'parallel_driver'])

        N = 10
        self.c0.N = N

        cid.workflow.add('c1')
        cid.add_parameter('c1.i')
        cid.add_response('c1.val')
        cid.case_inputs.c1.i = range(N)

        self.connect('c0.l', 'c1.l')


class V(VariableTree):
    l = List([])


class C0_vt(Component):
    vt = VarTree(V(), iotype='out')
    N = Int(10, iotype='in')

    def execute(self):
        self.vt.l = range(self.N)


class C1_vt(Component):
    vt = VarTree(V(), iotype='in')
    i = Int(0, iotype='in')
    val = Int(0, iotype='out')

    def execute(self):
        self.val = self.vt.l[self.i]


class A_vt(Assembly):
    def configure(self):
        self.add('c0', C0_vt())
        self.add('c1', C1_vt())

        cid = self.add('parallel_driver', CaseIteratorDriver())
        self.driver.workflow.add(['c0', 'parallel_driver'])

        N = 10
        self.c0.N = N

        cid.workflow.add(['c1'])
        cid.add_parameter('c1.i')
        cid.add_response('c1.val')
        cid.case_inputs.c1.i = range(N)

        self.connect('c0.vt', 'c1.vt')


class Rethore(unittest.TestCase):

    def test_l_sequential(self):
        # Sequential is base.
        logging.debug('')
        logging.debug('test_l_sequential')
        a = set_as_top(A_l())
        cid = a.parallel_driver
        cid.sequential = True
        a.run()
        sequential = [(cid.case_inputs.c1.i[i], cid.case_outputs.c1.val[i])
                      for i in range(len(cid.case_inputs.c1.i))]

    def test_l_concurrent(self):
        raise SkipTest("concurrent CaseIterDriver execution currently not supported")
        # Now run concurrent and verify.
        logging.debug('')
        logging.debug('test_l_concurrent')
        a = set_as_top(A_l())
        cid = a.parallel_driver
        cid.sequential = False
        a.run()
        concurrent = [(cid.case_inputs.c1.i[i], cid.case_outputs.c1.val[i])
                      for i in range(len(cid.case_inputs.c1.i))]

        self.assertEqual(concurrent, sequential)

    def test_vt_sequential(self):
        # Sequential is base.
        logging.debug('')
        logging.debug('test_vt_sequential')
        a = set_as_top(A_vt())
        cid = a.parallel_driver
        cid.sequential = True
        a.run()
        sequential = [(cid.case_inputs.c1.i[i], cid.case_outputs.c1.val[i])
                      for i in range(len(cid.case_inputs.c1.i))]

    def test_vt_concurrent(self):
        raise SkipTest("concurrent CaseIterDriver execution currently not supported")
        # Now run concurrent and verify.
        logging.debug('')
        logging.debug('test_vt_concurrent')
        a = set_as_top(A_vt())
        cid = a.parallel_driver
        cid.sequential = False
        a.run()
        concurrent = [(cid.case_inputs.c1.i[i], cid.case_outputs.c1.val[i])
                      for i in range(len(cid.case_inputs.c1.i))]

        self.assertEqual(concurrent, sequential)


class SimpleComp(Component):

    in1 = Float(6., iotype='in')
    in2 = Float(7., iotype='in')

    out1 = Float(iotype='out')
    out2 = Float(iotype='out')

    def execute(self):
        print self.get_pathname(), 'execute', self.in1, self.in2
        self.out1 = self.in1 + self.in2
        self.out2 = self.in1 * self.in2


class Aggregator(Component):

    in1 = Array(iotype='in')
    in2 = Array(iotype='in')
    in3 = Array(iotype='in')

    out1 = Array(iotype='out')

    def execute(self):
        print self.get_pathname(), 'execute', self.in1, self.in2, self.in3
        self.out1 = self.in1 + self.in2 + self.in3


class SampleAssembly(Assembly):

    def configure(self):

        self.add('a', SimpleComp())
        self.add('b', SimpleComp())
        self.add('c', SimpleComp())
        self.add('d', Aggregator())

        self.connect('a.out1', 'b.in1')
        self.connect('b.out1', 'c.in1')

        self.add('cid_driver', CaseIteratorDriver())
#        self.add('cid_driver', SimpleCaseIterDriver())

        #note, using "new" parameter interface that does not require low/high
        self.cid_driver.add_parameter('b.in2')
        self.cid_driver.add_parameter('c.in2')

        #tells the driver which values from the MP runs are of interest,
        # allows us to create extra variables only for the needed values
        #output arrays created on the driver on the fly, with no size
        # determined until runtime
        #variable names have no special meaning, just chosen for clarity here
        self.cid_driver.add_response('b.out1')
        self.cid_driver.add_response('b.out2')
        self.cid_driver.add_response('c.out1')

        #vtree inputs created on the fly based on parameters given
        #number of multi-point executions given at runtime based on length of
        # inputs, all inputs must be same length
        self.cid_driver.case_inputs.b.in2 = [1., 2., 3., 4., 5., 6.]
        self.cid_driver.case_inputs.c.in2 = [0., 1., 0., 1., 0., 1.]

        #d is a component that does mp_aggregation
        #NOTE: d is expecting arrays of equal length
        self.connect('cid_driver.case_outputs.b.out1', 'd.in1')
        self.connect('cid_driver.case_outputs.b.out2', 'd.in2')
        self.connect('cid_driver.case_outputs.c.out1', 'd.in3')

        #Options:
        #  1) d could  be a very simple component that requires all the array
        #     data to be pulled onto one processor
        #  2) d could be a more advanced component that works with distributed
        #     vectors to do aggregation operations (like sum or norm)
        #Possibly could make a setting or two different MPIMultiPoint drivers
        # to control this behavior.
        #One would require a standard array output. The other a distributed
        # vector. I'm not sure what the right answer is...

        self.driver.workflow.add(['a', 'cid_driver', 'd'])

        self.cid_driver.workflow.add(['b', 'c'])


class MultiPoint(unittest.TestCase):

    def test_multipoint(self):
        top = set_as_top(SampleAssembly())
        top.run()
        self.assertEqual(list(top.d.in1), [14., 15., 16., 17., 18., 19.])
        self.assertEqual(list(top.d.in2), [13., 26., 39., 52., 65., 78.])
        self.assertEqual(list(top.d.in3), [14., 16., 16., 18., 18., 20.])


class ConnectC(Component):
    i1 = Float(0., iotype='in')
    o1 = Float(iotype='out')

    def execute(self):
        self.o1 = self.i1 **2.

class ConnectA(Assembly):
    i1 = List([], iotype='in')
    o1 = List(iotype='out')

    def __init__(self, sequential):
        self.sequential = sequential
        super(ConnectA, self).__init__()

    def configure(self):
        self.add('c', ConnectC())
        self.add('driver', CaseIteratorDriver())
        self.driver.sequential = self.sequential
        self.driver.workflow.add('c')
        self.driver.add_parameter('c.i1')
        self.driver.add_response('c.o1')
        self.connect('i1', 'driver.case_inputs.c.i1')
        self.connect('driver.case_outputs.c.o1', 'o1')


class Connections(unittest.TestCase):

    def test_connections(self):
        print '---- sequential ----'
        a1 = set_as_top(ConnectA(True))
        a1.i1 = [float(i) for i in range(10)]
        a1.run()
        print a1.driver.case_inputs.c.i1
        print a1.driver.case_outputs.c.o1
        print a1.o1
        self.assertEqual(a1.o1, [float(v)**2 for v in range(10)])

        print '\n---- par ----'
        a1.i1 = [float(i) for i in range(5)]
        a1.driver.sequential = False
        a1.run()
        print a1.driver.case_inputs.c.i1
        print a1.driver.case_outputs.c.o1
        print a1.o1
        self.assertEqual(a1.o1, [float(v)**2 for v in range(5)])

        print '\n---- seq ----'
        a1.i1 = [float(i) for i in range(3)]
        a1.driver.sequential = True
        a1.run()
        print a1.driver.case_inputs.c.i1
        print a1.driver.case_outputs.c.o1
        print a1.o1
        self.assertEqual(a1.o1, [float(v)**2 for v in range(3)])


# Test bug reported by Frederik Zahle.

class Builder(Component):

    x0 = Float(iotype='in')
    y0 = Float(iotype='in')
    x = List(iotype='out')
    y = List(iotype='out')

    def execute(self):
        self.x = list(linspace(self.x0 - 2, self.x0 + 2, 5))
        self.y = list(linspace(self.y0 - 2, self.y0 + 2, 5))


class DummyComp(Component):

    x_in = Float(float('NaN'), iotype='in')
    x_out = Float(float('NaN'), iotype='out')
    y_in = Float(float('NaN'), iotype='in')
    y_out = Float(float('NaN'), iotype='out')

    def execute(self):
        self.x_out = self.x_in
        self.y_out = self.y_in


class Paraboloidish(Component):

    x = List(iotype='in')
    y = List(iotype='in')
    f_xy = Float(iotype='out')

    def execute(self):
        x_mean = mean(asarray(self.x))
        y_mean = mean(asarray(self.y))
        self.f_xy = (x_mean-3.0)**2 + x_mean*y_mean + (y_mean+4.0)**2 - 3.0
        print self.name, x_mean, y_mean, self.f_xy


class CIDAssembly(Assembly):

    def configure(self):
        self.add('p', DummyComp())

        cid = self.add('cid', CaseIteratorDriver())
        self.driver.workflow.add('cid')
        cid.workflow.add('p')

        #cid.sequential = False
        #cid.reload_model = False

        cid.add_parameter('p.x_in')
        cid.add_parameter('p.y_in')
        cid.add_response('p.x_out')
        cid.add_response('p.y_out')

        self.add('parab', Paraboloidish())
        self.driver.workflow.add('parab')

        self.create_passthrough('cid.case_inputs.p.x_in')
        self.create_passthrough('cid.case_inputs.p.y_in')

        self.connect('cid.case_outputs.p.x_out', 'parab.x')
        self.connect('cid.case_outputs.p.y_out', 'parab.y')

        self.create_passthrough('parab.f_xy')


class OptAssembly(Assembly):

    def configure(self):
        self.add('driver', SLSQPdriver())
        self.driver.gradient_options.force_fd = True
        self.driver.iout = 1
        self.driver.iprint = 3
        self.driver.maxiter = 100

        self.add('builder', Builder())
        self.driver.workflow.add('builder')

        self.add('cidasm', CIDAssembly())
        self.driver.workflow.add('cidasm')

        self.connect('builder.x', 'cidasm.x_in')
        self.connect('builder.y', 'cidasm.y_in')

        self.driver.add_parameter('builder.x0', low=-50, high=50)
        self.driver.add_parameter('builder.y0', low=-50, high=50)
        self.driver.add_constraint('builder.x0-builder.y0 >= 15.0')
        self.driver.add_objective('cidasm.f_xy')


class OptimizationTestCase(unittest.TestCase):

    def test_optimization(self):
        # Test that CID within an optimization works.
        top = OptAssembly()
        top.run()
        print 'objective', top.cidasm.f_xy
        assert_rel_error(self, top.cidasm.f_xy, -27.0833328304, 0.001)

        # Clean up after ourselves
        outfile = 'slsqp.out'
        if os.path.exists(outfile):
            os.remove(outfile)


# Test bug reported by Pierre-Elouan Rethore.

class PTComp(Component):

    i = Float(iotype='in')
    o = Float(iotype='out')

    def execute(self):
        self.o = self.i**2.


class PTReplacement(PTComp):

    i = Float(iotype='in')
    o = Float(iotype='out')

    def execute(self):
        self.o = self.i**2.+1.


class PTAssembly(Assembly):

    def configure(self):
        self.add('c', PTComp())
        self.add('driver', CaseIteratorDriver())
        self.driver.workflow.add(['c'])
        self.driver.add_parameter('c.i')
        self.driver.add_response('c.o')
        self.driver.case_inputs.c.i = [float(i) for i in range(10)]

class ParameterTarget(unittest.TestCase):

    def test_parameter_target(self):
        # Test that replacing a parameter target is handled.
        a1 = set_as_top(PTAssembly())
        a1.run()
        self.assertEqual(a1.driver.case_outputs.c.o,
                         [0., 1., 4., 9., 16., 25., 36., 49., 64., 81.])

    def test_parameter_target_replace(self):
        a2 = set_as_top(PTAssembly())
        a2.replace('c', PTReplacement())
        # connecting the replacement 'c' to the driver results in the
        # case_inputs.c.i being set to [], so we need to recreate the list of inputs
        a2.driver.case_inputs.c.i = [float(i) for i in range(10)]
        a2.run()
        self.assertEqual(a2.driver.case_outputs.c.o,
                         [1., 2., 5., 10., 17., 26., 37., 50., 65., 82.])


# Test bug reported by Frederik Zahle. Sequential version would fail.

class OutputVT(VariableTree):

    a = Float()

class AComp(Component):

    inp = Float(iotype='in')
    out = VarTree(OutputVT(), iotype='out')

    def execute(self):
        self.out.a = 2 * self.inp

class CaseIter(Assembly):

    def configure(self):
        self.add('driver', CaseIteratorDriver())
        self.add('acomp', AComp())
        self.driver.workflow.add('acomp')
        self.driver.add_parameter('acomp.inp')
        self.driver.add_response('acomp.out')
        self.driver.case_inputs.acomp.inp = [0., 1., 2.]

class Zahle(unittest.TestCase):

    def test_sequential(self):
        top = CaseIter()
        top.run()
        out = top.driver.case_outputs.acomp.out
        out = [out[i].a for i in range(len(out))]
        self.assertEqual(out, [0., 2., 4.])

    def test_concurrent(self):
        top = CaseIter()
        top.driver.sequential = False
        top.run()
        out = top.driver.case_outputs.acomp.out
        out = [out[i].a for i in range(len(out))]
        self.assertEqual(out, [0., 2., 4.])


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.lib.drivers')
    sys.argv.append('--cover-erase')
    nose.runmodule()
