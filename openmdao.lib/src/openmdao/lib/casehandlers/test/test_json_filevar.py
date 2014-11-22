import os.path
import re
import sys
import unittest

from cStringIO import StringIO

from openmdao.main import __version__

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.datatypes.api import File, Float
from openmdao.lib.drivers.api import CONMINdriver
from openmdao.lib.casehandlers.api import JSONCaseRecorder


class WriteFile(Component):
    x=Float(1.0,iotype='in')
    y=Float(2.0,iotype='in')
    file_out=File('x.in',iotype='out')

    def execute(self):
        with open(self.file_out.abspath(),'w') as f:
            f.write('{:7.3f}\n'.format(self.x))
            f.write('{:7.3f}\n'.format(self.y))


class ReadFile(Component):
    file_in=File(None,iotype='in',allow_none=True)
    x=Float(iotype='out')
    y=Float(iotype='out')

    def execute(self):
        with open(self.file_in.abspath(),'r') as f:
            self.x=float(f.readline())
            self.y=float(f.readline())


class Paraboloid(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """

    # set up interface to the framework
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(0.0, iotype='out', desc='F(x,y)')

    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
            Minimum: x = 6.6667; y = -7.3333
        """

        x = self.x
        y = self.y

        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0


class EvalParaboloid(Assembly):
    def configure(self):
        self.add('reader',ReadFile())
        self.add('func',Paraboloid())
        self.connect('reader.x','func.x')
        self.connect('reader.y','func.y')
        self.create_passthrough('reader.file_in')
        self.create_passthrough('func.f_xy')
        self.driver.workflow.add(['reader','func'])


class Top(Assembly):
    def configure(self):
        self.add('writer',WriteFile())
        self.add('c2',EvalParaboloid())
        self.connect('writer.file_out','c2.file_in')

        self.add('driver',CONMINdriver())
        self.driver.workflow.add(['writer','c2'])
        self.driver.add_parameter('writer.x', low=-50., high=50.,)
        self.driver.add_parameter('writer.y', low=-50., high=50.,)
        self.driver.add_objective('c2.f_xy')
        self.driver.conmin_diff=True


class TestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Top())

    def test_file_vars(self):
        sout = StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]

        self.top.recording_options.save_problem_formulation=True
        self.top.recording_options.includes=[
            'writer.x', 'writer.y', 'c2.f_xy',
            'writer.file_out', 'c2.file_in'
        ]

        self.top.run()

        # with open('paraboloid.new', 'w') as out:
        #     out.write(sout.getvalue())
        self.verify(sout, 'paraboloid.json')

    def verify(self, sout, filename):
        lines = sout.getvalue().split('\n')

        directory = os.path.dirname(__file__)
        path = os.path.join(directory, filename)
        with open(path, 'r') as inp:
            expected = inp.read().split('\n')

        for i in range(len(expected)):
            expect = expected[i]
            if expect.startswith('"__length_'):
                self.assertTrue(lines[i].startswith('"__length_'))
            elif expect.startswith(', "__length_'):
                self.assertTrue(lines[i].startswith(', "__length_'))
            elif expect.startswith('    "OpenMDAO_Version":'):
                expect_fixed = expect[:25] + __version__ + '", '
                self.assertEqual(lines[i], expect_fixed)
            elif expect.startswith('    "_driver_id":'):
                self.assertTrue(lines[i].startswith('    "_driver_id":'))
            elif expect.startswith('    "_id":'):
                self.assertTrue(lines[i].startswith('    "_id":'))
            elif expect.startswith('    "_parent_id":'):
                self.assertTrue(lines[i].startswith('    "_parent_id":'))
            elif expect.startswith('    "uuid":'):
                self.assertTrue(lines[i].startswith('    "uuid":'))
            elif expect.startswith('    "timestamp":'):
                self.assertTrue(lines[i].startswith('    "timestamp":'))
            elif expect.startswith('            "pcomp_name":'):
                self.assertTrue(lines[i].startswith('            "pcomp_name":'))
            elif expect.startswith('            "high":'):
                value = re.match('.*:([^,]*),', lines[i]).group(1)
                if value != ' null':
                    self.assertEqual(int(value), sys.maxint)
            elif expect.startswith('            "low":'):
                value = re.match('.*:([^,]*),', lines[i]).group(1)
                if value not in (' null', ' 0'):
                    self.assertEqual(int(value), -sys.maxint)
            elif expect.startswith('        "_pseudo_1":') or \
                 expect.startswith('        "_pseudo_0":'):
                expect = float(re.match('.*:([^,]*),', expect).group(1))
                value = float(re.match('.*:([^,]*),', lines[i]).group(1))
                # Multiple representations of zero...
                self.assertEqual(value, expect)
            elif expect.startswith('        "writer.x":'):
                self.assertTrue(lines[i].startswith('        "writer.x":'))
            elif expect.startswith('        "writer.y":'):
                self.assertTrue(lines[i].startswith('        "writer.y":'))
            elif expect.startswith('            "platform":'):
                self.assertTrue(lines[i].startswith('            "platform":'))
            elif not expect.startswith('    "graph":'):
                if expect.startswith('        "c2.f_xy":') and '{' not in expect:
                    expect = float(re.match('.*:([^,]*),', expect).group(1))
                    value = float(re.match('.*:([^,]*),', lines[i]).group(1))
                else:
                    self.assertEqual(lines[i], expect)

    def tearDown(self):
        if os.path.exists("x.in"):
            os.remove('x.in')
        self.top = None


if __name__ == '__main__':
    unittest.main()
