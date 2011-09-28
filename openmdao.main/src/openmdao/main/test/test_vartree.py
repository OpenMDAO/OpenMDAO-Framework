import glob
import os
import unittest

from enthought.traits.trait_base import not_none

from openmdao.main.api import Container, Component, Assembly, VariableTree, set_as_top, FileRef
from openmdao.lib.datatypes.api import Float, Slot, File

class DumbVT3(VariableTree):
    def __init__(self):
        super(DumbVT3, self).__init__()
        self.add('a', Float(1., units='ft'))
        self.add('b', Float(12., units='inch'))
        self.add('data', File())
        
class DumbVT2(VariableTree):
    def __init__(self):
        super(DumbVT2, self).__init__()
        self.add('x', Float(-1.))
        self.add('y', Float(-2.))
        self.add('data', File())
        self.add('vt3', DumbVT3())
    
class DumbVT(VariableTree):
    def __init__(self):
        super(DumbVT, self).__init__()
        self.add('vt2', DumbVT2())
        self.add('v1', Float(1.))
        self.add('v2', Float(2.))
        self.add('data', File())

class SimpleComp(Component):
    cont_in = Slot(DumbVT, iotype='in')
    cont_out = Slot(DumbVT, iotype='out')
    
    def __init__(self, *args, **kwargs):
        super(SimpleComp, self).__init__(*args, **kwargs)
        self.add('cont_in', DumbVT())
        self.add('cont_out', DumbVT())
    
    def execute(self):
        self.cont_out.v1 = self.cont_in.v1 + 1.0
        self.cont_out.v2 = self.cont_in.v2 + 1.0
        self.cont_out.vt2.x = self.cont_in.vt2.x + 1.0
        self.cont_out.vt2.y = self.cont_in.vt2.y + 1.0
        self.cont_out.vt2.vt3.a = self.cont_in.vt2.vt3.a
        self.cont_out.vt2.vt3.b = self.cont_in.vt2.vt3.b

        if self.cont_in.data is not None:
            with self.cont_in.data.open() as inp:
                filename = '%s.data.vt' % self.name
                with open(filename, 'w') as out:
                    out.write(inp.read())
                self.cont_out.data = FileRef(filename, self)

        if self.cont_in.vt2.data is not None:
            with self.cont_in.vt2.data.open() as inp:
                filename = '%s.data.vt2' % self.name
                with open(filename, 'w') as out:
                    out.write(inp.read())
                self.cont_out.vt2.data = FileRef(filename, self)

        if self.cont_in.vt2.vt3.data is not None:
            with self.cont_in.vt2.vt3.data.open() as inp:
                filename = '%s.data.vt3' % self.name
                with open(filename, 'w') as out:
                    out.write(inp.read())
                self.cont_out.vt2.vt3.data = FileRef(filename, self)

    def get_vals(self, iotype):
        if iotype == 'in':
            cont = self.cont_in
        else:
            cont = self.cont_out
        return [
            cont.v1,
            cont.v2,
            cont.vt2.x,
            cont.vt2.y,
            cont.vt2.vt3.a,
            cont.vt2.vt3.b,
            ]

    def get_files(self, iotype):
        if iotype == 'in':
            cont = self.cont_in
        else:
            cont = self.cont_out
        return [
            cont.data,
            cont.vt2.data,
            cont.vt2.vt3.data,
            ]


class NamespaceTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        obj = self.asm.add('scomp1', SimpleComp())
        self.asm.add('scomp2', SimpleComp())
        self.asm.driver.workflow.add(['scomp1','scomp2'])

        with self.asm.dir_context:
            filename = 'top.data.vt'
            with open(filename, 'w') as out:
                out.write('vt data\n')
            obj.cont_in.data = FileRef(filename, self.asm)

            filename = 'top.data.vt2'
            with open(filename, 'w') as out:
                out.write('vt2 data\n')
            obj.cont_in.vt2.data = FileRef(filename, self.asm)

            filename = 'top.data.vt3'
            with open(filename, 'w') as out:
                out.write('vt3 data\n')
            obj.cont_in.vt2.vt3.data = FileRef(filename, self.asm)

    def tearDown(self):
        for name in glob.glob('*.data.vt*'):
            os.remove(name)

    def _check_values(self, expected, actual):
        for e, a in zip(expected, actual):
            self.assertEqual(e,a)

    def _check_files(self, expected, actual):
        for e, a in zip(expected, actual):
            with e.open() as inp:
                edata = inp.read()
            with a.open() as inp:
                adata = inp.read()
            self.assertEqual(edata,adata)

    def test_pass_container(self):
        #scomp1                   scomp2
            #cont_in         /------->cont_in
                #v1          |           v1
                #v2          |           v2
                #vt2         |           vt2
                    #x       |               x
                    #y       |               y
                    #vt3     |               vt3
                       #a    |                  a
                       #b    |                  b
            #cont_out--------/        cont_out
                #v1                      v1
                #v2                      v2
                #vt2                     vt2
                    #x                       x
                    #y                       y
                    #vt3                     vt3
                       #a                       a
                       #b                       b
        self.asm.connect('scomp1.cont_out', 'scomp2.cont_in')
        self.asm.scomp1.cont_out.v1 = 99.
        self.asm.scomp1.cont_out.v2 = 88.
        self.asm.scomp1.cont_out.vt2.x = 999.
        self.asm.scomp1.cont_out.vt2.y = 888.
        self.asm.scomp1.cont_out.vt2.vt3.a = 9999.
        self.asm.scomp1.cont_out.vt2.vt3.b = 8888.
        self.asm.run()
        self.assertFalse(self.asm.scomp2.cont_in is self.asm.scomp1.cont_out)
        self._check_values(self.asm.scomp1.get_vals('out'),
                           self.asm.scomp2.get_vals('in'))
        # 'in/out' set for end-to-end check.
        self._check_files(self.asm.scomp1.get_files('in'),
                          self.asm.scomp2.get_files('out'))
        try:
            self.asm.connect('scomp1.cont_out.v1', 'scomp2.cont_in.v2')
        except Exception as err:
            self.assertEqual(str(err), ": 'scomp2.cont_in' is already connected to source 'scomp1.cont_out'")
        else:
            self.fail("exception expected")
        
    def test_connect_subvartree(self):
        self.asm.connect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        self.asm.run()
        self.assertEqual(self.asm.scomp1.cont_out.v1, 
                         self.asm.scomp2.cont_in.v1+1.0)
        self.assertEqual(self.asm.scomp1.cont_out.v2, 
                         self.asm.scomp2.cont_in.v2+1.0)
        # [2:] indicates that all values from vt2 on down should agree
        self._check_values(self.asm.scomp1.get_vals('out')[2:],
                           self.asm.scomp2.get_vals('in')[2:])
        # 'in/out' set for end-to-end check.
        self.assertEqual(self.asm.scomp2.get_files('out')[0], None)
        self._check_files(self.asm.scomp1.get_files('in')[1:],
                          self.asm.scomp2.get_files('out')[1:])

    def test_connect_subvar(self):
        self.asm.connect('scomp1.cont_out.v1', 'scomp2.cont_in.v2')
        self.asm.connect('scomp1.cont_out.v2', 'scomp2.cont_in.v1')
        self.asm.run()
        self.assertEqual(self.asm.scomp1.cont_out.v1, 
                         self.asm.scomp2.cont_in.v2)
        self.assertEqual(self.asm.scomp1.cont_out.v2, 
                         self.asm.scomp2.cont_in.v2+1.0)

    def test_connect_subsubvar(self):
        self.asm.connect('scomp1.cont_out.vt2.vt3.a', 'scomp2.cont_in.vt2.vt3.b')
        self.asm.run()
        self.assertAlmostEqual(12.0*self.asm.scomp1.cont_out.vt2.vt3.a, 
                               self.asm.scomp2.cont_in.vt2.vt3.b)
        try:
            self.asm.connect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        except Exception as err:
            self.assertEqual(str(err), 
                "'vt2.vt3.b' is already connected to source 'parent.parent.scomp1.cont_out.vt2.vt3.a'")
        else:
            self.fail("exception expected")
        
        try:
            self.asm.connect('scomp1.cont_out', 'scomp2.cont_in')
        except Exception as err:
            self.assertEqual(str(err), 
                "cont_in.vt2.vt3.b is already connected to source parent.scomp1.cont_out.vt2.vt3.a")
        else:
            self.fail("exception expected")
            
        self.asm.disconnect('scomp1.cont_out.vt2.vt3.a', 'scomp2.cont_in.vt2.vt3.b')
        # now this should be allowed
        self.asm.connect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        self.asm.disconnect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        # and now this
        self.asm.connect('scomp1.cont_out', 'scomp2.cont_in')
        
    def test_callbacks(self):
        # verify that setting a var nested down in a VariableTree hierarchy will
        # notify the parent Component that an input has changed
        self.asm.run()
        self.assertEqual(self.asm.scomp1._call_execute, False)
        self.asm.scomp1.cont_in.vt2.vt3.a = 5.0
        self.assertEqual(self.asm.scomp1._call_execute, True)
        self.asm.run()
        self.assertEqual(self.asm.scomp1._call_execute, False)
        self.asm.scomp1.cont_in.vt2.x = -5.0
        self.assertEqual(self.asm.scomp1._call_execute, True)
        self.asm.run()
        
        # setting something in an output VariableTree should NOT set _call_execute
        self.asm.scomp1.cont_out.vt2.vt3.a = 55.0
        self.assertEqual(self.asm.scomp1._call_execute, False)
        
    def test_pathname(self):
        vt = self.asm.scomp2.cont_out.vt2.vt3
        self.assertEqual('scomp2.cont_out.vt2.vt3', vt.get_pathname())
        self.asm.scomp1.cont_in.vt2.vt3 = vt
        self.assertEqual('scomp1.cont_in.vt2.vt3', 
                         self.asm.scomp1.cont_in.vt2.vt3.get_pathname())
        
    def test_iotype(self):
        vt = self.asm.scomp2.cont_out.vt2.vt3
        self.assertEqual(vt._iotype, 'out')
        self.asm.scomp1.cont_in.vt2.vt3 = vt
        self.assertEqual(vt._iotype, 'in')
        
        dvt = DumbVT()
        self.assertEqual(dvt._iotype, '')
        self.asm.scomp2.cont_out = dvt
        self.assertEqual(self.asm.scomp2.cont_out._iotype, 'out')
        self.assertEqual(self.asm.scomp2.cont_out.vt2._iotype, 'out')
        self.assertEqual(self.asm.scomp2.cont_out.vt2.vt3._iotype, 'out')
        
    def test_items(self):
        vtvars = ['v1','v2','vt2','data']
        vt2vars = ['vt2.x','vt2.y','vt2.vt3','vt2.data']
        vt3vars = ['vt2.vt3.a','vt2.vt3.b','vt2.vt3.data']
        
        result = dict(self.asm.scomp1.cont_out.items(iotype='out'))
        self.assertEqual(set(result.keys()), set(vtvars))
        result = dict(self.asm.scomp1.cont_out.items(recurse=True, iotype='out'))
        self.assertEqual(set(result.keys()), set(vtvars+vt2vars+vt3vars))
        result = dict(self.asm.scomp1.cont_out.items(iotype='in'))
        self.assertEqual(set(result.keys()), set([]))
        result = dict(self.asm.scomp1.cont_out.items(iotype=None))
        self.assertEqual(set(result.keys()), set([]))
        result = dict(self.asm.scomp1.cont_out.items(iotype=not_none))
        self.assertEqual(set(result.keys()), set(vtvars))
        
        result = dict(self.asm.scomp1.cont_in.items(iotype='in'))
        self.assertEqual(set(result.keys()), set(vtvars))
        result = dict(self.asm.scomp1.cont_in.items(recurse=True, iotype='in'))
        self.assertEqual(set(result.keys()), set(vtvars+vt2vars+vt3vars))
        result = dict(self.asm.scomp1.cont_in.items(iotype='out'))
        self.assertEqual(set(result.keys()), set([]))
    
    
if __name__ == "__main__":
    unittest.main()

