import os.path
import tempfile
import shutil
import unittest
import json

from cStringIO import StringIO


from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.datatypes.api import FileRef, File, Float
from openmdao.lib.drivers.api import CONMINdriver
from openmdao.lib.casehandlers.api import JSONCaseRecorder


class WriteFile(Component):
    x=Float(1.0,iotype='in')
    y=Float(2.0,iotype='in')
    file_out=File(FileRef('x.in'), iotype='out')

    def execute(self):
        with open(self.file_out.path,'w') as f:
            f.write('{:7.3f}\n'.format(self.x))
            f.write('{:7.3f}\n'.format(self.y))


class ReadFile(Component):
    file_in=File(None,iotype='in',allow_none=True)
    x=Float(iotype='out')
    y=Float(iotype='out')

    def execute(self):
        with open(self.file_in.path,'r') as f:
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
        self.path = os.path.abspath(os.path.dirname(__file__))
        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='test_json_filevar-')
        os.chdir(self.tempdir)
        self.top = set_as_top(Top())

    def tearDown(self):
        self.top = None
        os.chdir(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

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

    def _dict_iter(self, dct):
        for k,v in dct.items():
            if isinstance(v, dict):
                for kk,vv in self._dict_iter(v):
                    yield (kk, vv)
            else:
                yield (k, v)

    def verify(self, sout, filename):
        path = os.path.join(self.path, filename)
        with open(path, 'r') as inp:
            old_json = json.load(inp)

        new_json = json.loads(sout.getvalue())

        old = list(self._dict_iter(old_json))
        new = list(self._dict_iter(new_json))

        if len(old) != len(new):
            self.fail("Number of items (%d) != number of items expected (%d)" %
                      (len(old), len(new)))

        ignore = set(['uuid', 'OpenMDAO_Version', '_id',
                      '_driver_id', '_parent_id', 'timestamp', 'pcomp_name'])

        for (oldname, oldval), (newname, newval) in zip(old, new):
            if oldname.startswith('__length_'):
                continue
            if oldname in ignore: # don't care if these match
                continue
            if oldname == newname:
                self.assertAlmostEqual(oldname, newname, '%s != %s' % (oldname, newname))
            else:
                self.assertEqual(oldname, newname) # just raises an exception


if __name__ == '__main__':
    unittest.main()
