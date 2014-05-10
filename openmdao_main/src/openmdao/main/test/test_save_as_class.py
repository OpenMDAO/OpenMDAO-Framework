# pylint: disable-msg=C0111,C0103

import sys
import unittest
import tempfile
import shutil

from traits.api import TraitError
from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Float, Str, List
from openmdao.util.decorators import add_delegate
from openmdao.main.hasobjective import HasObjective
from openmdao.util.fileutil import onerror


class Multiplier(Component):
    rval_in = Float(iotype='in')
    rval_out = Float(iotype='out')
    mult = Float(iotype='in')

    def __init__(self):
        super(Multiplier, self).__init__()
        self.rval_in = 4.
        self.rval_out = 6.
        self.mult = 1.5

    def execute(self):
        self.rval_out = self.rval_in * self.mult


class SaveAsClassTestCase(unittest.TestCase):

    def setUp(self):
        pass
        #self.tdir = tempfile.mkdtemp()

    def tearDown(self):
        pass
        #shutil.rmtree(self.tdir, onerror=onerror)

    #def test_save_as_class(self):

        #top = set_as_top(Assembly())
        #comp1 = top.add('comp1', Multiplier())
        #comp2 = top.add('comp2', Multiplier())

        #top.driver.workflow.add(['comp1', 'comp2'])

        #top.comp1.mult = 2.0
        #top.comp2.mult = 4.0
        #top.connect('comp1.rval_out', 'comp2.rval_in')
        #top.comp1.rval_in = 5.0

        #model_to_package(top, 'Foo', '1.0', destdir=tdir)
        ##ci = top.get_configinfo()
        ##ci.save_as_class(sys.stdout, 'Foo')


if __name__ == "__main__":
    unittest.main()
