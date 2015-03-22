# pylint: disable=C0111,C0103

import os.path
import sys
import unittest
import StringIO
import nose
import copy

from traits.api import HasTraits

from openmdao.util import eggsaver as constants
from openmdao.main.container import Container, \
                                    get_default_name, find_name, \
                                    find_trait_and_value, _get_entry_group, \
                                    create_io_traits
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.main.variable import Variable
from openmdao.main.datatypes.api import Instance, Float, Int, Bool, List, Dict
from openmdao.util.testutil import make_protected_dir, assert_raises

# Various Pickle issues arise only when this test runs as the main module.
# This is used to detect when we're the main module or not.
MODULE_NAME = __name__


class DumbTrait(Variable):
    def validate(self, obj, name, value):
        """Validation for the PassThroughTrait"""
        if self.validation_trait:
            return self.validation_trait.validate(obj, name, value)
        return value


class MyContainer(Container):
    uncertain = Instance(NormalDistribution, iotype="out")

    def __init__(self):
        super(MyContainer, self).__init__()
        self.uncertain = NormalDistribution()
        self.add('dyntrait', Float(9., desc='some desc'))


class MyBuilderContainer(Container):

    def contains(self, path):
        return path != 'does_not_exist'

    def build_trait(self, ref_name, iotype=None, trait=None):
        if iotype is None:
            iostat = 'in'
        else:
            iostat = iotype

        if isinstance(iostat, dict):
            metadata = iostat
        else:
            metadata = dict(iotype=iostat)
        metadata['ref_name'] = ref_name

        if trait is None:
            if ref_name.startswith('f'):
                trait = Float(0.0, **metadata)
            elif ref_name.startswith('i'):
                trait = Int(0, **metadata)
            else:
                self.raise_exception("can't determine type of variable '%s'"
                                         % ref_name, RuntimeError)
        return trait


class ContainerTestCase(unittest.TestCase):

    def setUp(self):
        """This sets up the following hierarchy of Containers:

                       root
                       /  \
                     c1    c2
                          /  \
                        c21  c22
                             /
                          c221
                          /
                        number
        """

        self.root = Container()
        self.root.add('c1', Container())
        self.root.add('c2', Container())
        self.root.c2.add('c21', Container())
        self.root.c2.add('c22', Container())
        self.root.c2.c22.add('c221', Container())
        self.root.c2.c22.c221.add('number', Float(3.14, iotype='in'))

    def tearDown(self):
        """this teardown function will be called after each test"""
        self.root = None

    def test_deepcopy(self):
        cont = MyContainer()
        self.assertEqual(cont.dyntrait, 9.)
        ccont = copy.deepcopy(cont)
        self.assertEqual(ccont.dyntrait, 9.)
        cont.dyntrait = 12.
        ccont2 = copy.deepcopy(cont)
        self.assertEqual(ccont2.dyntrait, 12.)

    def test_build_trait(self):
        mbc = MyBuilderContainer()
        obj_info = ['f_in', ('f_out_internal', 'f_out', 'out'),
                    'i_in', 'i_out',
                    ('b_out_internal', 'b_out', 'out', Bool()),
                    ('i_has_metadata', '', dict(iotype='in', low=-1, high=9))
            ]
        create_io_traits(mbc, obj_info)
        create_io_traits(mbc, 'foobar')
        self.assertTrue(mbc.get_trait('b_out').is_trait_type(Bool))
        self.assertTrue(mbc.get_trait('f_out').is_trait_type(Float))
        self.assertEqual(mbc.get_trait('f_out').iotype, 'out')
        self.assertTrue(mbc.get_trait('i_in').is_trait_type(Int))
        self.assertEqual(mbc.get_trait('f_in').iotype, 'in')
        self.assertTrue(mbc.get_trait('foobar').is_trait_type(Float))
        self.assertEqual(mbc.get_trait('foobar').iotype, 'in')
        self.assertTrue(mbc.get_trait('i_has_metadata').is_trait_type(Int))
        self.assertEqual(mbc.get_trait('i_has_metadata').iotype, 'in')
        self.assertEqual(mbc.get_trait('i_has_metadata').low, -1)
        self.assertEqual(mbc.get_trait('i_has_metadata').high, 9)

        code = "create_io_traits(mbc, [{}])"
        msg = ": create_io_traits cannot add trait {}"
        assert_raises(self, code, globals(), locals(), RuntimeError, msg)

        code = "create_io_traits(mbc, ('f_in', 'f.in'))"
        msg = ": Can't create 'f.in' because it's a dotted pathname"
        assert_raises(self, code, globals(), locals(), NameError, msg)

        code = "create_io_traits(mbc, ('f2_in', 'f_in'))"
        msg = ": Can't create 'f_in' because it already exists"
        assert_raises(self, code, globals(), locals(), RuntimeError, msg)

        code = "create_io_traits(mbc, 'does_not_exist')"
        msg = ": Can't create trait for 'does_not_exist' because it wasn't found"
        assert_raises(self, code, globals(), locals(), AttributeError, msg)

        cont = Container()
        cont.contains = lambda name: True
        code = "create_io_traits(cont, 'xyzzy')"
        msg = ": build_trait()"
        assert_raises(self, code, globals(), locals(), NotImplementedError, msg)

    def test_find_trait_and_value(self):
        class MyClass(object):
            pass

        class MyHT(HasTraits):
            pass

        obj = MyClass()
        obj.sub = MyClass()
        obj.sub.sub = MyHT()
        obj.sub.csub = Container()
        obj.a = 1
        obj.sub.b = 2
        obj.sub.sub.c = 3
        obj.sub.csub.add('d', Float(4, iotype='in'))
        result = find_trait_and_value(obj, 'sub.sub.c')
        self.assertEqual(result[0].type, 'python')
        self.assertEqual(result[1], 3)
        result = find_trait_and_value(obj, 'sub.csub.d')
        self.assertEqual(result[0].type, 'trait')
        self.assertEqual(result[1], 4)
        try:
            result = find_trait_and_value(obj, 'sub.foo')
        except AttributeError as err:
            self.assertEqual(str(err), "'MyClass' object has no attribute 'foo'")
        else:
            self.fail("expected AttributeError")

    def test_attrib_metadata(self):
        cont = MyContainer()
        io = cont.get_metadata('uncertain.mu', 'iotype')
        self.assertEqual(io, 'out')

    def test_get_default_name(self):
        class MyClass(object):
            pass
        parent = MyClass()
        pname = get_default_name(parent, None)
        self.assertEqual(pname, 'myclass1')

        for i in range(3):
            obj = MyClass()
            oname = get_default_name(obj, parent)
            setattr(parent, oname, obj)
            self.assertEqual(oname, "myclass%s" % (i + 1))

    def test_find_name(self):
        class MyClass(object):
            pass
        obj = MyClass()
        obj2 = MyClass()
        self.assertEqual(find_name(obj, obj2), '')
        setattr(obj, 'foo', obj2)
        self.assertEqual(find_name(obj, obj2), 'foo')

    def test_get_entry_group(self):
        class MyClass(object):
            pass
        self.assertEqual(_get_entry_group(MyClass()), None)
        self.assertEqual(_get_entry_group(Container()), 'openmdao.container')

    def test_add_non_container(self):
        foo = Container()
        foo.add('non_container', 'some string')
        self.assertEqual(foo.non_container, 'some string')

    def test_pathname(self):
        self.root.add('foo', Container())
        self.root.foo.add('foochild', Container())
        self.assertEqual(self.root.foo.foochild.get_pathname(), 'foo.foochild')

    def test_add_bad_name(self):
        bad_names = ['parent', 'self', 'for', 'if', 'while', 'sin', 'cos', 'tan']
        for bad in bad_names:
            try:
                self.root.add(bad, Container())
            except Exception as err:
                self.assertEqual(str(err), ": '%s' is a reserved or invalid name" % bad)
            else:
                self.fail("name '%s' should be illegal" % bad)

    def test_get(self):
        obj = self.root.get('c2.c21')
        self.assertEqual(obj.get_pathname(), 'c2.c21')
        num = self.root.get('c2.c22.c221.number')
        self.assertEqual(num, 3.14)

    def test_add_trait_w_subtrait(self):
        obj = Container()
        obj.add('lst', List([1, 2, 3], iotype='in'))
        obj.add('dct', Dict({}, iotype='in'))

    def test_get_attribute(self):
        self.assertEqual(self.root.get('c2.c22.c221').get_trait('number').iotype,
                         'in')

    def test_full_items(self):
        lst = map(lambda x: x[0], self.root.items(recurse=True))
        self.assertEqual(lst,
            ['c2', 'c2.c22', 'c2.c22.c221', 'c2.c22.c221.number', 'c2.c21', 'c1'])

        items = [(x[0], isinstance(x[1], Container) or str(x[1]))
                    for x in self.root.items(recurse=True)]

        # values of True in the list below just indicate that the value
        # is a Container
        self.assertEqual(items, [('c2', True),
                                 ('c2.c22', True),
                                 ('c2.c22.c221', True),
                                 ('c2.c22.c221.number', '3.14'),
                                 ('c2.c21', True),
                                 ('c1', True)])

    def test_default_naming(self):
        cont = Container()
        cont.add('container1', Container())
        cont.add('container2', Container())
        self.assertEqual(get_default_name(Container(), cont), 'container3')
        self.assertEqual(get_default_name(Container(), None), 'container1')

    def test_bad_get(self):
        try:
            x = self.root.bogus
        except AttributeError, err:
            self.assertEqual(str(err), "'Container' object has no attribute 'bogus'")
        else:
            self.fail('AttributeError expected')

    def test_iteration(self):
        names = [x.get_pathname() for n, x in self.root.items(recurse=True)
                                         if isinstance(x, Container)]
        self.assertEqual(set(names),
                         set(['c1', 'c2', 'c2.c21',
                              'c2.c22', 'c2.c22.c221']))

        names = [x.get_pathname() for n, x in self.root.items()
                                         if isinstance(x, Container)]
        self.assertEqual(set(names), set(['c1', 'c2']))

        names = [x.get_pathname() for n, x in self.root.items(recurse=True)
                                 if isinstance(x, Container) and x.parent == self.root]
        self.assertEqual(set(names), set(['c1', 'c2']))

        names = [x.get_pathname() for n, x in self.root.items(recurse=True)
                                 if isinstance(x, Container) and x.parent == self.root.c2]
        self.assertEqual(set(names), set(['c2.c21', 'c2.c22']))

    def test_iteration2(self):
        # Had been skipping some traits of multiple instances.
        class Ext(Container):
            resources = Dict(iotype='in')

        top = Ext()
        top.add('ext2', Ext())
        sub = top.add('sub', Container())
        sub.add('ext3', Ext())

        names = [n for n, x in top.items(recurse=True)]
        expected = ['ext2', 'ext2.resources', 'resources',
                    'sub', 'sub.ext3', 'sub.ext3.resources']
        self.assertEqual(sorted(names), expected)

    # TODO: all of these save/load test functions need to do more checking
    #       to verify that the loaded thing is equivalent to the saved thing

    #def test_save_load_yaml(self):
        #output = StringIO.StringIO()
        #c1 = Container()
        #c1.add('c2', Container())
        #c1.save(output, constants.SAVE_YAML)

        #inp = StringIO.StringIO(output.getvalue())
        #newc1 = Container.load(inp, constants.SAVE_YAML)

    #def test_save_load_libyaml(self):
        #output = StringIO.StringIO()
        #c1 = Container()
        #c1.add('c2', Container())
        #c1.save(output, constants.SAVE_LIBYAML)

        #inp = StringIO.StringIO(output.getvalue())
        #newc1 = Container.load(inp, constants.SAVE_LIBYAML)

    def test_save_load_cpickle(self):
        output = StringIO.StringIO()
        c1 = Container()
        c1.add('c2', Container())
        c1.add('list_in', List(Float, iotype='in'))
        c1.list_in = [1., 2., 3.]
        self.assertEqual(c1.list_in, [1., 2., 3.])
        c1.save(output)

        inp = StringIO.StringIO(output.getvalue())
        newc1 = Container.load(inp)
        self.assertEqual(newc1.list_in, [1., 2., 3.])

        # The List fixup issue occurs on the second save/load.
        output = StringIO.StringIO()
        newc1.save(output)
        inp = StringIO.StringIO(output.getvalue())
        newerc1 = Container.load(inp)
        self.assertEqual(newerc1.list_in, [1., 2., 3.])

    def test_save_load_pickle(self):
        output = StringIO.StringIO()
        c1 = Container()
        c1.add('c2', Container())
        c1.save(output, constants.SAVE_PICKLE)

        inp = StringIO.StringIO(output.getvalue())
        newc1 = Container.load(inp, constants.SAVE_PICKLE)

    def test_save_bad_format(self):
        output = StringIO.StringIO()
        c1 = Container()
        try:
            c1.save(output, 'no-such-format')
        except RuntimeError, exc:
            msg = ": Can't save object using format 'no-such-format'"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected RuntimeError')

    def test_save_bad_filename(self):
# TODO: get make_protected_dir() to work on Windows.
        if sys.platform == 'win32':
            raise nose.SkipTest("make_protected_dir() doesn't work on Windows.")

        c1 = Container()
        directory = make_protected_dir()
        path = os.path.join(directory, 'illegal')
        try:
            c1.save(path)
        except IOError, exc:
            msg = ": Can't save to '%s': Permission denied" % path
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected IOError')
        finally:
            os.rmdir(directory)

    def test_save_bad_method(self):
        # This test exercises handling references to unbound methods defined
        # in __main__.  Because of this, it only does it's job if this is the
        # main module (not run as part of a larger suite in the buildout dir).
        output = StringIO.StringIO()
        c1 = Container()
        c1.unbound_thing = ContainerTestCase.test_save_bad_method
        try:
            c1.save(output)
        except RuntimeError, exc:
            msg = ": _pickle_method: <unbound method ContainerTestCase" \
                  ".test_save_bad_method> with module __main__ (None)"
            self.assertEqual(str(exc), msg)
        else:
            if MODULE_NAME == '__main__':
                self.fail('Expected RuntimeError')

    def test_load_bad_format(self):
        try:
            Container.load(StringIO.StringIO(''), 'no-such-format')
        except RuntimeError, exc:
            msg = "Can't load object using format 'no-such-format'"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected RuntimeError')

    def test_load_nofile(self):
        try:
            Container.load('no-such-file')
        except ValueError, exc:
            msg = "Bad state filename 'no-such-file'."
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected ValueError')

    def test_recursion(self):
        a = Container()  # No ancestors.
        assert_raises(self, "a.add('a', a)",
                      globals(), locals(), ValueError,
                      ": add would cause container recursion")

        b = a.add('b', Container())  # Have ancestors.
        assert_raises(self, "b.add('a', a)",
                      globals(), locals(), ValueError,
                      "b: add would cause container recursion")

    def test_set_metadata(self):
        c = Container()
        c.add_trait('inp', List(range(1000), ddcomp_start=0, ddcomp_end=-1))
        self.assertEqual(c.get_metadata('inp', 'ddcomp_start'), 0)
        self.assertEqual(c.get_metadata('inp', 'ddcomp_end'), -1)

        c.set_metadata('inp', 'ddcomp_start', 10)
        c.set_metadata('inp', 'ddcomp_end', 20)
        self.assertEqual(c.get_metadata('inp', 'ddcomp_start'), 10)
        self.assertEqual(c.get_metadata('inp', 'ddcomp_end'), 20)

        assert_raises(self, "c.set_metadata('inp', 'iotype', 'out')",
                      globals(), locals(), TypeError,
                      ": Can't set iotype on inp, read-only")


if __name__ == "__main__":
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()
