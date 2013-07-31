import glob
import os
import unittest

from traits.trait_base import not_none

from openmdao.main.api import Component, Assembly, VariableTree, \
                              set_as_top, SimulationRoot
from openmdao.main.datatypes.api import Float, File, FileRef, List, VarTree
from openmdao.main.case import flatten_obj

from openmdao.util.testutil import assert_raises


class DumbVT3(VariableTree):

    a = Float(1., units='ft')
    b = Float(12., units='inch')
    data = File()


class DumbVT2(VariableTree):

    x = Float(-1.)
    y = Float(-2.)
    data = File()
    vt3 = VarTree(DumbVT3())


class BadVT2(VariableTree):

    x = Float(-1.)
    y = Float(-2.)
    data = File()
    vt3 = DumbVT3()


class DumbVT(VariableTree):

    v1 = Float(1., desc='vv1')
    v2 = Float(2., desc='vv2')
    data = File()
    vt2 = VarTree(DumbVT2())


class SimpleComp(Component):
    cont_in = VarTree(DumbVT(), iotype='in')
    cont_out = VarTree(DumbVT(), iotype='out')

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
        # SimulationRoot is static and so some junk can be left
        # over from other tests when running under nose, so
        # set it to cwd here just to be safe
        SimulationRoot.chroot(os.getcwd())
        self.asm = set_as_top(Assembly())
        obj = self.asm.add('scomp1', SimpleComp())
        self.asm.add('scomp2', SimpleComp())
        self.asm.driver.workflow.add(['scomp1', 'scomp2'])

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
            self.assertEqual(e, a)

    def _check_files(self, expected, actual):
        for e, a in zip(expected, actual):
            with e.open() as inp:
                edata = inp.read()
            with a.open() as inp:
                adata = inp.read()
            self.assertEqual(edata, adata)

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

        # Check set_attributes on the vartrees
        attrs = self.asm.scomp1.cont_in.get_attributes()
        self.assertTrue("Inputs" in attrs.keys())
        self.assertTrue({'name': 'v1',
                         'id': '.v1',
                         'indent': 0,
                         'value': 1.0,
                         'high': None,
                         'connected': '',
                         'valid': 'false',
                         'low': None,
                         'type': 'float',
                         'desc': 'vv1'} in attrs['Inputs'])
        self.assertTrue({'name': 'v2',
                         'id': '.v2',
                         'indent': 0,
                         'value': 2.0,
                         'high': None,
                         'connected': '',
                         'valid': 'false',
                         'low': None,
                         'type': 'float',
                         'desc': 'vv2'} in attrs['Inputs'])
        # The number shall be 11 becuase of recursion, and also including
        # file variables
        self.assertEqual(len(attrs['Inputs']), 11)
        attrs = self.asm.scomp1.cont_out.get_attributes()
        self.assertTrue("Outputs" in attrs.keys())
        self.assertTrue({'name': 'v1',
                         'id': '.v1',
                         'indent': 0,
                         'value': 2.0,
                         'high': None,
                         'connected': '',
                         'valid': 'false',
                         'low': None,
                         'type': 'float',
                         'desc': 'vv1'} in attrs['Outputs'])
        self.assertTrue({'name': 'v2',
                         'id': '.v2',
                         'indent': 0,
                         'value': 3.0,
                         'high': None,
                         'connected': '',
                         'valid': 'false',
                         'low': None,
                         'type': 'float',
                         'desc': 'vv2'} in attrs['Outputs'])
        self.assertEqual(len(attrs['Outputs']), 11)

        # Now connect
        try:
            self.asm.connect('scomp1.cont_out.v1', 'scomp2.cont_in.v2')
        except Exception as err:
            self.assertEqual(str(err), ": Can't connect 'scomp1.cont_out.v1' to 'scomp2.cont_in.v2': 'scomp2.cont_in' is already connected to source 'scomp1.cont_out'")
        else:
            self.fail("exception expected")

    def test_connect_subvartree(self):
        self.asm.connect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        self.asm.run()
        self.assertEqual(self.asm.scomp1.cont_out.v1,
                         self.asm.scomp2.cont_in.v1 + 1.0)
        self.assertEqual(self.asm.scomp1.cont_out.v2,
                         self.asm.scomp2.cont_in.v2 + 1.0)
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
                         self.asm.scomp2.cont_in.v2 + 1.0)

    def test_connect_subsubvar(self):
        self.asm.connect('scomp1.cont_out.vt2.vt3.a', 'scomp2.cont_in.vt2.vt3.b')
        self.asm.run()
        self.assertAlmostEqual(12.0 * self.asm.scomp1.cont_out.vt2.vt3.a,
                               self.asm.scomp2.cont_in.vt2.vt3.b)
        try:
            self.asm.connect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        except Exception as err:
            self.assertEqual(str(err),
                ": Can't connect 'scomp1.cont_out.vt2' to 'scomp2.cont_in.vt2': 'scomp2.cont_in.vt2.vt3.b' is already connected to source 'scomp1.cont_out.vt2.vt3.a'")
        else:
            self.fail("exception expected")

        try:
            self.asm.connect('scomp1.cont_out', 'scomp2.cont_in')
        except Exception as err:
            self.assertEqual(str(err),
                ": Can't connect 'scomp1.cont_out' to 'scomp2.cont_in': 'scomp2.cont_in.vt2.vt3.b' is already connected to source 'scomp1.cont_out.vt2.vt3.a'")
        else:
            self.fail("exception expected")

        self.asm.disconnect('scomp1.cont_out.vt2.vt3.a', 'scomp2.cont_in.vt2.vt3.b')
        # now this should be allowed
        self.asm.connect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        self.asm.disconnect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        # and now this
        self.asm.connect('scomp1.cont_out', 'scomp2.cont_in')
        self.asm.disconnect('scomp1.cont_out', 'scomp2.cont_in')

        self.asm.connect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        self.asm.disconnect('scomp1.cont_out', 'scomp2.cont_in')

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
        vtvars = ['v1', 'v2', 'vt2', 'data']
        vt2vars = ['vt2.x', 'vt2.y', 'vt2.vt3', 'vt2.data']
        vt3vars = ['vt2.vt3.a', 'vt2.vt3.b', 'vt2.vt3.data']

        result = dict(self.asm.scomp1.cont_out.items(iotype='out'))
        self.assertEqual(set(result.keys()), set(vtvars))
        result = dict(self.asm.scomp1.cont_out.items(recurse=True, iotype='out'))
        self.assertEqual(set(result.keys()), set(vtvars + vt2vars + vt3vars))
        result = dict(self.asm.scomp1.cont_out.items(iotype='in'))
        self.assertEqual(set(result.keys()), set([]))
        result = dict(self.asm.scomp1.cont_out.items(iotype=None))
        self.assertEqual(set(result.keys()), set([]))
        result = dict(self.asm.scomp1.cont_out.items(iotype=not_none))
        self.assertEqual(set(result.keys()), set(vtvars))

        result = dict(self.asm.scomp1.cont_in.items(iotype='in'))
        self.assertEqual(set(result.keys()), set(vtvars))
        result = dict(self.asm.scomp1.cont_in.items(recurse=True, iotype='in'))
        self.assertEqual(set(result.keys()), set(vtvars + vt2vars + vt3vars))
        result = dict(self.asm.scomp1.cont_in.items(iotype='out'))
        self.assertEqual(set(result.keys()), set([]))

    def test_flatten(self):
        dvt = DumbVT()
        self.assertEqual(set(flatten_obj('foo', dvt)),
                         set([('foo.vt2.vt3.a', 1.), ('foo.vt2.vt3.b', 12.),
                              ('foo.vt2.x', -1.), ('foo.vt2.y', -2.),
                              ('foo.v1', 1.), ('foo.v2', 2.)]))

    def test_nesting(self):
        # Check direct nesting in class definition.
        code = 'vt2 = BadVT2()'
        msg = 'Nested VariableTrees are not supported,' \
              ' please wrap BadVT2.vt3 in a VarTree'
        assert_raises(self, code, globals(), locals(), TypeError, msg,
                      use_exec=True)

        # Check direct nesting via add().
        vt3 = DumbVT3()
        vt3.add('ok', VarTree(DumbVT3()))
        code = "vt3.add('bad', DumbVT3())"
        msg = ': a VariableTree may only contain Variables or VarTrees'
        assert_raises(self, code, globals(), locals(), TypeError, msg)


class Level2Tree(VariableTree):
    lev2float = Float()

class Level1Tree(VariableTree):
    lev1float = Float()
    lev2 = VarTree(Level2Tree()) # no iotype

class TopTree(VariableTree):
    lev1 = VarTree(Level1Tree())  # no iotype
    topfloat = Float()


class NestedTreeComp(Component):
    top_tree_in = VarTree(TopTree(), iotype='in')

    def execute(self):
        pass

class NestedVTTestCase(unittest.TestCase):
    
    def test_nested_iotype(self):
        # nested tree
        comp = NestedTreeComp()
        
        self.assertEqual(comp.top_tree_in.lev1.lev2._iotype, '')
        self.assertEqual(comp.top_tree_in.lev1.lev2.iotype, 'in')
        self.assertEqual(comp.top_tree_in.lev1.lev2._iotype, 'in')
        
        attr = comp.top_tree_in.get_attributes()
        outputs = attr.get('Outputs', [])
        self.assertEqual(outputs, [])
        inputs = attr['Inputs']
        self.assertEqual(set([d['name'] for d in inputs]), 
                         set(['topfloat','lev1','lev1float','lev2','lev2float']))
        
        newvt = comp.top_tree_in.copy()
        newvt._iotype = 'out'
        
        attr = newvt.get_attributes()
        inputs = attr.get('Inputs', [])
        outputs = attr.get('Outputs', [])
        self.assertEqual(inputs, [])
        self.assertEqual(set([d['name'] for d in outputs]), 
                         set(['topfloat','lev1','lev1float','lev2','lev2float']))
        
        self.assertEqual(newvt.lev1.lev2.iotype, 'out')
        newvt._iotype = 'in'
        self.assertEqual(newvt.iotype, 'in')
        self.assertEqual(newvt.lev1.lev2.iotype, 'in')
        
    def test_nested_iotype_passthrough(self):
        # nested tree
        asm = set_as_top(Assembly())
        comp = asm.add("comp", NestedTreeComp())
        asm.create_passthrough('comp.top_tree_in')
        
        attr = asm.top_tree_in.get_attributes()
        outputs = attr.get('Outputs', [])
        self.assertEqual(outputs, [])
        inputs = attr['Inputs']
        self.assertEqual(set([d['name'] for d in inputs]), 
                         set(['topfloat','lev1','lev1float','lev2','lev2float']))
        

        newvt = asm.top_tree_in.copy()
        newvt._iotype = 'out'
        
        attr = newvt.get_attributes()
        inputs = attr.get('Inputs', [])
        outputs = attr.get('Outputs', [])
        self.assertEqual(inputs, [])
        self.assertEqual(set([d['name'] for d in outputs]), 
                         set(['topfloat','lev1','lev1float','lev2','lev2float']))
        
        attr = comp.top_tree_in.get_attributes()
        outputs = attr.get('Outputs', [])
        self.assertEqual(outputs, [])
        inputs = attr['Inputs']
        self.assertEqual(set([d['name'] for d in inputs]), 
                         set(['topfloat','lev1','lev1float','lev2','lev2float']))
        

class ListConnectTestCase(unittest.TestCase):

    def test_connect(self):
        class Vars(VariableTree):
            f1 = Float()
            f2 = Float()

        class TestAsm(Assembly):

            f_in = Float(iotype='in')

            def configure(self):
                self.add('c2', TestComponent2())
                self.driver.workflow.add('c2')

                # Construct list with only one element for demo purposes
                self.c2.vtlist = [Vars()]
                self.connect('f_in', 'c2.vtlist[0].f1')
                self.c2.vtlist[0].f2 = 90

                self.create_passthrough('c2.f_out')

        class TestComponent2(Component):

            vtlist = List(trait=Vars, value=[], iotype='in')
            f_out = Float(iotype='out')

            def execute(self):
                self.f_out = self.vtlist[0].f1 + self.vtlist[0].f2

        top = set_as_top(Assembly())
        # Init of model
        test_asm = TestAsm()
        top.add('asm', test_asm)
        top.driver.workflow.add('asm')
        test_asm.f_in = 10

        test_asm.run()

        self.assertEqual(100, test_asm.f_out)


if __name__ == "__main__":
    unittest.main()
