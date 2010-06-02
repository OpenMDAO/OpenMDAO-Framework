"""
Test for XYPlotter
"""

import unittest
import tempfile
import StringIO
import time

from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.api import DBCaseIterator, DBCaseRecorder, DumpCaseRecorder, ListCaseIterator
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver
from openmdao.lib.components.xyplotter import XYplotter

    
class XYplotterTestCase(unittest.TestCase):

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        top.add_container('comp1', ExecComp(exprs=['z=x*y+100']))
        top.add_container('comp2', ExecComp(exprs=['z=x*.4']))
        top.connect('comp1.z', 'comp2.x')
        driver = top.add_container('driver', SimpleCaseIterDriver())
        
        plotter = top.add_container('plotter', XYplotter())
        plotter.title = "Foobar"
        #plotter.add_line(y="comp1.z", line_type='bo-')
        plotter.add_line(x="comp1.x", y="comp1.z", line_type='bo-')
        plotter.add_line(x='comp1.x', y="comp2.z", line_type='rD-', label='blah')
        
        # now create some Cases
        outputs = [('comp1.z', None, None), ('comp2.z', None, None)]
        cases = []
        for i in range(10):
            inputs = [('comp1.x', None, i), ('comp1.y', None, i*2)]
            cases.append(Case(inputs=inputs, outputs=outputs, ident='case%s'%i))
        driver.iterator = ListCaseIterator(cases)

    def test_xyplotter(self):
        self.top.driver.recorder = DumpCaseRecorder(None)
        self.top.run()
        # TODO: find a way to actually test a plot...
        #self.top.plotter.display()
        
if __name__ == '__main__':
    unittest.main()

