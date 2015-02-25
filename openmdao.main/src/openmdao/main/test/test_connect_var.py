#test_connect_var.py
"""
    Testing connecting different variable types to each other.
"""


import unittest
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Array, Float, Int, Str, Bool, Enum

class Oneout(Component):
    """ A simple output component    """

    ratio1 = Float(3.54, iotype='out',
                   desc='Float Variable')
    ratio2 = Int(9, iotype='out',
                   desc='Integer variable')
    ratio3 = Bool(True, iotype='out',
                   desc='Boolean Variable')
    ratio4 = Float(1.03, iotype='out',
                   desc='Float variable ',units='cm')
    ratio5 = Str('05678', iotype='out',
                   desc='string variable')
    ratio6 = Enum(27, (0,3,9,27), iotype='out', desc='some enum')
    unit = Float(12.0, units='inch', iotype='out')
    no_unit = Float(12.0, iotype='out')

    arrout = Array(dtype=float, default_value=[1, 2, 3], iotype='out')

    def execute(self):
        """
           execute
        """
        # print '%s.execute()' % self.get_pathname()



class Oneinp(Component):
    """ A simple input component    """

    ratio1 = Float(2., iotype='in',
                   desc='Float   Variable')
    ratio2 = Int(2, iotype='in',
                   desc='Int Variable')
    ratio3 = Bool(False, iotype='in',
                   desc='Float Variable')
    ratio4 = Float(1.03, iotype='in',
                   desc='Float variable ',units='ft')
    ratio5 = Str('01234', iotype='in',
                   desc='string variable')
    ratio6 = Enum(0, (0,3,11,27), iotype='in', desc='some enum')
    unit = Float(0.0, units='ft', iotype='in')
    no_unit = Float(0.0, iotype='in')

    arrinp = Array(dtype=float, default_value=[42, 13, 0], iotype='in')

    def execute(self):
        """
           execute
        """
        # print '%s.execute()' % self.get_pathname()


class VariableTestCase(unittest.TestCase):

    def setUp(self):
        """ this function is used to test each type..."""
        self.top = set_as_top(Assembly())
        self.top.add('oneinp', Oneinp())
        self.top.add('oneout', Oneout())
        self.top.driver.workflow.add(['oneinp', 'oneout'])

    def tearDown(self):
        self.top = None

    def test_undefined_srcexpr_var(self):
        try:
            self.top.connect('oneout.fake', 'oneinp.ratio1')
        except AttributeError as err:
            expected = ": "\
                       "Can't connect 'oneout.fake' to 'oneinp.ratio1': : "\
                       "'oneout' has no variable 'fake'"

            self.assertEqual(str(err), expected)

        try:
            self.top.add('a', Assembly())
            self.top.a.add('oneinp', Oneinp())
            self.top.a.connect('fake', 'oneinp.ratio1')
        except AttributeError as err:
            expected = "a: "\
                       "Can't connect 'fake' to 'oneinp.ratio1': : "\
                       "'a' has no variable 'fake'"

            self.assertEqual(str(err), expected)

        try:
            self.top.connect('fake', 'oneinp.ratio1')
        except AttributeError as err:
            expected = ": "\
                       "Can't connect 'fake' to 'oneinp.ratio1': : "\
                       "top level assembly has no variable 'fake'"

            self.assertEqual(str(err), expected)



    def test_undefined_destexpr_var(self):
        try:
            self.top.connect('oneout.ratio1', 'oneinp.fake')
        except AttributeError as err:
            expected = ": "\
                       "Can't connect 'oneout.ratio1' to 'oneinp.fake': : "\
                       "'oneinp' has no variable 'fake'"

            self.assertEqual(str(err), expected)

        try:
            self.top.connect('oneout.ratio1', 'fake')
        except AttributeError as err:
            expected = ": "\
                       "Can't connect 'oneout.ratio1' to 'fake': : "\
                       "top level assembly has no variable 'fake'"

            self.assertEqual(str(err), expected)

        try:
            self.top.add('a', Assembly())
            self.top.a.add('oneout', Oneout())
            self.top.a.connect('oneout.ratio1', 'fake')
        except AttributeError as err:
            expected = "a: "\
                       "Can't connect 'oneout.ratio1' to 'fake': : "\
                       "'a' has no variable 'fake'"

            self.assertEqual(str(err), expected)

    def test_var1(self):
        #  connect to same type variables....
        self.top.connect('oneout.ratio1','oneinp.ratio1')      # float to float
        self.top.connect('oneout.ratio2','oneinp.ratio2')      # int   to int
        self.top.connect('oneout.ratio3','oneinp.ratio3')      # Bool  to Bool
        self.top.connect('oneout.ratio4','oneinp.ratio4')      # float with units to float with unit
        self.top.connect('oneout.ratio5','oneinp.ratio5')      # Str   to Str
        self.top.connect('oneout.ratio6','oneinp.ratio6')      # Enum  to Enum (int valued)
        self.top.run()
        self.assertEqual(3.54,self.top.oneinp.ratio1)
        self.assertEqual(9 ,self.top.oneinp.ratio2)
        self.assertEqual(True,self.top.oneinp.ratio3)
        self.assertAlmostEqual(0.033792,self.top.oneinp.ratio4,5)
        self.assertEqual('05678',self.top.oneinp.ratio5)
        self.assertEqual(27,self.top.oneinp.ratio6)

    def test_var2(self):
        self.top.oneout.ratio2 = 11
        self.top.connect('oneout.ratio2','oneinp.ratio1')      # int  to  Float
        self.top.oneout.ratio3 = True
        self.top.connect('oneout.ratio3','oneinp.ratio2')      # Bool to  int
        self.top.connect('oneout.ratio2','oneinp.ratio6')      # Int  to  Enum (int valued)
        self.top.run()
        self.assertEqual(11.0,self.top.oneinp.ratio1)
        self.assertEqual(True,self.top.oneinp.ratio2)
        self.assertEqual(11,self.top.oneinp.ratio6)

    def test_var3(self):
        self.top.oneout.ratio3 = False
        self.top.connect('oneout.ratio3','oneinp.ratio1')      # Bool to  Float
        self.top.connect('oneout.ratio3','oneinp.ratio2')      # Bool to  int
        self.top.run()
        self.assertEqual(0.0,self.top.oneinp.ratio1)
        self.assertEqual(False,self.top.oneinp.ratio2)

    def test_var3a(self):
        self.top.connect('oneout.ratio6','oneinp.ratio2')      # Enum  to Int
        self.top.run()
        self.assertEqual(27,self.top.oneinp.ratio2)

    def test_var4(self):
        self.top.oneout.ratio1 = 12.0
        try:
            self.top.connect('oneout.ratio1','oneinp.ratio2')  # float to int
            self.top._setup()
        except Exception, err:
            msg = "but a value of 12.0 <type 'float'> was specified"
            self.assertTrue( msg in str(err))
        else:
            self.fail('Exception Expected')

    def test_var5(self):
        self.top.oneout.ratio1 = 12.0
        try:
            self.top.connect('oneout.ratio1','oneinp.ratio3')  # float to Bool
        except Exception, err:
            msg = ": Can't connect 'oneout.ratio1' to 'oneinp.ratio3': : " +\
                  "The 'ratio3' trait of an Oneinp instance must be a"\
                  " boolean, but a value of 12.0 <type 'float'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('Exception Expected')

    def test_var6(self):
        self.top.oneout.ratio1 = 12.0
        try:
            self.top.connect('oneout.ratio1','oneinp.ratio5')  # float to Str
        except Exception, err:
            msg = ": Can't connect 'oneout.ratio1' to 'oneinp.ratio5': : " +\
                  "The 'ratio5' trait of an Oneinp instance must be a"\
                  " string, but a value of 12.0 <type 'float'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('Exception Expected')

    def test_var7(self):
        self.top.oneout.ratio2 = 20
        try:
            self.top.connect('oneout.ratio2','oneinp.ratio3')  # int to Bool
        except Exception, err:
            msg = ": Can't connect 'oneout.ratio2' to 'oneinp.ratio3': : " +\
                  "The 'ratio3' trait of an Oneinp instance must be a"\
                  " boolean, but a value of 20 <type 'int'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('Exception Expected')

    def test_var8(self):
        self.top.oneout.ratio2 = 20
        try:
            self.top.connect('oneout.ratio2','oneinp.ratio5')  # int to Str
        except Exception, err:
            msg = ": Can't connect 'oneout.ratio2' to 'oneinp.ratio5': : " +\
                  "The 'ratio5' trait of an Oneinp instance must be a"\
                  " string, but a value of 20 <type 'int'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('Exception Expected')

    def test_var9(self):
        self.top.oneout.ratio3 = True
        try:
            self.top.connect('oneout.ratio3','oneinp.ratio5')  # Bool to Str
        except Exception, err:
            msg = ": Can't connect 'oneout.ratio3' to 'oneinp.ratio5': : " +\
                  "The 'ratio5' trait of an Oneinp instance must be a"\
                  " string, but a value of True <type 'bool'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('Exception Expected')

    def test_var10(self):
        self.top.oneout.ratio5 = '55555'
        try:
            self.top.connect('oneout.ratio5','oneinp.ratio2')  # Str to int
        except Exception, err:
            msg = "a value of 55555 <type 'str'> was specified"
            self.assertTrue( msg in str(err))
        else:
            self.fail('Exception Expected')

    def test_var11(self):
        self.top.oneout.ratio5 = '55555'
        try:
            self.top.connect('oneout.ratio5','oneinp.ratio1')  # Str to Float
        except Exception, err:
            msg = ": Can't connect 'oneout.ratio5' to 'oneinp.ratio1': : oneinp: " + \
                  "Variable 'ratio1' must be a float, but a value of 55555" + \
                      " <type 'str'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('Exception Expected')

    def test_var12(self):
        self.top.oneout.ratio5 = '55555'
        try:
            self.top.connect('oneout.ratio5','oneinp.ratio3')  # Str to Bool
        except Exception, err:
            msg = ": Can't connect 'oneout.ratio5' to 'oneinp.ratio3': : " +\
                  "The 'ratio3' trait of an Oneinp instance must be a boolean, but a"\
                  " value of '55555' <type 'str'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('Exception Expected')

    def test_var13_units(self):
        self.top.connect('oneout.unit','oneinp.no_unit')      # Bool to  Float
        self.top.connect('oneout.no_unit','oneinp.unit')      # Bool to  int
        self.top.run()
        self.assertEqual(12.0,self.top.oneinp.no_unit)
        self.assertEqual(12.0,self.top.oneinp.unit)

    def _parse_list(self, liststr):
        liststr = liststr[1:len(liststr)-2]
        return set([s.strip("'") for s in liststr.split(', ') if s.strip()])


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.main')
    nose.runmodule()
