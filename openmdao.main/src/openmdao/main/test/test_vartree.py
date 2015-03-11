import copy
import glob
import os
import tempfile
import shutil

import unittest

from traits.trait_base import not_none

from openmdao.main.api import Component, Assembly, VariableTree, \
                              set_as_top, SimulationRoot
from openmdao.main.datatypes.api import Array, Bool, Enum, Float, File, \
                                        FileRef, Int, List, Str, VarTree, \
                                        Instance
from openmdao.main.case import flatten_obj

from openmdao.util.testutil import assert_raises


class DumbVT3(VariableTree):

    a = Float(1., units='ft')
    b = Float(12., units='inch')
    data = File()

class DumbVT3arr(VariableTree):

    a = Float(1., units='ft')
    b = Float(12., units='inch')
    arr = Array([1,2,3,4,5])
    data = File()

class DumbVT2(VariableTree):

    x = Float(-1.)
    y = Float(-2.)
    data = File()
    vt3 = VarTree(DumbVT3())

class DumbVT2arr(VariableTree):

    x = Float(-1.)
    y = Float(-2.)
    data = File()
    vt3 = VarTree(DumbVT3arr())

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

class DumbVTarr(VariableTree):

    v1 = Float(1., desc='vv1')
    v2 = Float(2., desc='vv2')
    data = File()
    vt2 = VarTree(DumbVT2arr())



class SimpleComp(Component):
    cont_in = VarTree(DumbVT(), iotype='in')
    cont_out = VarTree(DumbVT(), iotype='out')

    def __init__(self):
        super(SimpleComp, self).__init__()
        self._dirty = True
        self._set_input_callback('cont_in')

    def execute(self):
        self._dirty = False
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

    def _input_updated(self, *args, **kwargs):
        super(SimpleComp, self)._input_updated(*args, **kwargs)
        self._dirty = True


class NamespaceTestCase(unittest.TestCase):

    def setUp(self):
        # SimulationRoot is static and so some junk can be left
        # over from other tests when running under nose, so
        # set it here just to be safe
        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='omdao-')
        os.chdir(self.tempdir)
        SimulationRoot.chroot(self.tempdir)

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
        os.chdir(self.startdir)
        SimulationRoot.chroot(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

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

        # Now connect
        self.asm.connect('scomp1.cont_out.v1', 'scomp2.cont_in.v2')
        try:
            self.asm._setup()
        except Exception as err:
            self.assertEqual(str(err),
                ": Can't connect 'scomp1.cont_out.v1' to 'scomp2.cont_in.v2': :"
                " 'scomp2.cont_in' is already connected to 'scomp1.cont_out'")
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

    def test_list_all_vars(self):
        self.assertEqual(set(self.asm.scomp1.cont_out.list_all_vars()),
                         set(['cont_out.v1', 'cont_out.v2', 'cont_out.vt2.vt3.a',
                              'cont_out.vt2.vt3.b', 'cont_out.vt2.vt3.data',
                              'cont_out.vt2.data', 'cont_out.vt2.y', 'cont_out.vt2.x',
                              'cont_out.data']))

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
        self.asm.connect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')
        try:
            self.asm._setup()
        except Exception as err:
            self.assertEqual(str(err),
                ": Can't connect 'scomp1.cont_out.vt2' to 'scomp2.cont_in.vt2': :"
                " 'scomp2.cont_in.vt2.vt3.b' is already connected to"
                " '_pseudo_1.out0'")
        else:
            self.fail("exception expected")

        self.asm.disconnect('scomp1.cont_out.vt2', 'scomp2.cont_in.vt2')

        self.asm.connect('scomp1.cont_out', 'scomp2.cont_in')
        try:
            self.asm._setup()
        except Exception as err:
            self.assertEqual(str(err),
                ": Can't connect 'scomp1.cont_out' to 'scomp2.cont_in': :"
                " 'scomp2.cont_in.vt2.vt3.b' is already connected to"
                " '_pseudo_2.out0'")
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
        self.assertEqual(self.asm.scomp1._dirty, False)
        self.asm.scomp1.cont_in.vt2.vt3.a = 5.0
        self.assertEqual(self.asm.scomp1._dirty, True)
        self.asm.run()
        self.assertEqual(self.asm.scomp1._dirty, False)
        self.asm.scomp1.cont_in.vt2.x = -5.0
        self.assertEqual(self.asm.scomp1._dirty, True)
        self.asm.run()

        # setting something in an output VariableTree should NOT set _call_execute
        self.asm.scomp1.cont_out.vt2.vt3.a = 55.0
        self.assertEqual(self.asm.scomp1._dirty, False)

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
                              ('foo.v1', 1.), ('foo.v2', 2.),
                              ('foo.vt2.vt3.data', ''),
                              ('foo.vt2.data', ''), ('foo.data', '')]))

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
        # No iotype when creating TopTree.
        top = TopTree()
        self.assertEqual(top._iotype, '')
        self.assertEqual(top.iotype, '')
        self.assertEqual(top.lev1._iotype, '')
        self.assertEqual(top.lev1.iotype, '')
        self.assertEqual(top.lev1.lev2._iotype, '')
        self.assertEqual(top.lev1.lev2.iotype, '')

        # nested tree input -- iotype propagated all the way through.
        comp = NestedTreeComp()

        self.assertEqual(comp.top_tree_in._iotype, 'in')
        self.assertEqual(comp.top_tree_in.iotype, 'in')
        self.assertEqual(comp.top_tree_in.lev1._iotype, 'in')
        self.assertEqual(comp.top_tree_in.lev1.iotype, 'in')
        self.assertEqual(comp.top_tree_in.lev1.lev2._iotype, 'in')
        self.assertEqual(comp.top_tree_in.lev1.lev2.iotype, 'in')

        newvt = comp.top_tree_in.copy()
        newvt._iotype = 'out'

        self.assertEqual(newvt.lev1.lev2.iotype, 'out')
        newvt._iotype = 'in'
        self.assertEqual(newvt.iotype, 'in')
        self.assertEqual(newvt.lev1.lev2.iotype, 'in')


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

        top.run()

        self.assertEqual(100, test_asm.f_out)

    def test_connect2(self):
        class VT(VariableTree):

            x = Float(iotype='in')
            y = Float(iotype='in')

        class C(Component):

            x = Float(iotype='in')
            out = Float(iotype='out')

            def execute(self):
                self.out = 2*self.x

        class A(Assembly):

            vt = VarTree(VT(), iotype='in')

            def configure(self):
                self.add('c', C())
                self.driver.workflow.add(['c'])
                self.connect('vt.x', 'c.x')
                self.create_passthrough('c.out')

        a = A()
        a.vt.x = 1.0
        a.vt.y = 7.0

        a.run()

        self.assertEqual(a.out, 2.0)


class DeepCopyTestCase(unittest.TestCase):

    def test_deepcopy(self):
        # Check that complex tree is truly copied.
        # There had been a bug where added bodies were shared.

        class Simulation(VariableTree):
            time_stop = Float(300)
            solvertype = Int(1)
            convergence_limits = List([1.0e3, 1.0, 0.7])
            eig_out = Bool(False)

        class Mann(VariableTree):
            turb_base_name = Str('turb')
            std_scaling = Array([1.0, 0.7, 0.5])

        class Wind(VariableTree):
            horizontal_input = Enum(1, (0, 1))
            mann = VarTree(Mann())

        class BeamStructure(VariableTree):
            s = Array()

        class Body(VariableTree):
            body_name = Str('body')
            beam_structure = List(BeamStructure)

        class BodyList(VariableTree):
            def add_body(self, body_name, b):
                b.body_name = body_name
                self.add(body_name, VarTree(b))

        class VarTrees(VariableTree):
            sim = VarTree(Simulation())   # Single level.
            wind = VarTree(Wind())        # Static multilevel.
            bodies = VarTree(BodyList())  # Dynamic multilevel.

        a = VarTrees()
        a.name = 'A'
        a.bodies.add_body('the_body', Body())
        b = a.copy()
        errors = DeepCopyTestCase.checker(a, b)
        self.assertEqual(errors, 0)

    @staticmethod
    def checker(cont1, cont2, indent=0):
        errors = 0
        prefix = ' ' * (4*indent)
        print '%schecking' % prefix, cont1.get_pathname(), id(cont1), '0x%08x' % id(cont1)
        print '%s      vs' % prefix, cont2.get_pathname(), id(cont2), '0x%08x' % id(cont2)
        if id(cont1) == id(cont2):
            print '%s   ' % prefix, cont1.get_pathname(), 'is', cont2.get_pathname()
            errors += 1
        for name in sorted(cont1.list_containers()):
            sub1 = getattr(cont1, name)
            if hasattr(cont2, name):
                sub2 = getattr(cont2, name)
                errors += DeepCopyTestCase.checker(sub1, sub2, indent+1)
            else:
                print '%s   ' % prefix, cont2.get_pathname(), 'is missing', name
                errors += 1
        return errors

    def test_2nd_copy(self):
        # Test that copy of copy is valid (had 'lost' REGION00_v).

        class Region(VariableTree):
            thickness = Float()
            angle = Float()

        class StData(VariableTree):
            s = Float()

            def add_region(self, name):
                self.add(name + '_i', Instance(Region()))
                self.add(name + '_v', VarTree(Region()))
                self.add(name + '_f', Float())

        s1 = StData()
        s1.add_region('REGION00')
        s2 = copy.deepcopy(s1)
        s3 = copy.deepcopy(s2)
        keys = sorted([key for key in s3.__dict__ if not key.startswith('_')])
        expected = ['REGION00_f', 'REGION00_i', 'REGION00_v', 's']
        self.assertEqual(keys, expected)


if __name__ == "__main__":
    unittest.main()
