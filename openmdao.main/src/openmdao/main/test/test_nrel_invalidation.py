import unittest

from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float

from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial


class Comp(Component):
    """ Just sums inputs. """

    myin = Float(0, iotype='in')
    myout = Float(iotype='out')
    myotherin = Float(0, iotype='in')

    def execute(self):
        self.myout = self.myin + self.myotherin


class Comp1(Assembly):
    """ A framework 'component'. """

    myin1 = Float(1, iotype='in')
    myout1 = Float(iotype='out')
    myotherin = Float(5, iotype='in')

    def configure(self):
        super(Comp1, self).configure()
        self.add('comp', Comp())
        self.driver.workflow.add('comp')

    def execute(self):
        self.comp.myin = self.myin1
        self.comp.myotherin = self.myotherin
        super(Comp1, self).execute()
        self.myout1 = self.comp.myout


class Comp2(Assembly):
    """ A framework 'component'. """
  
    myin2 = Float(2, iotype='in')
    myout2 = Float(iotype='out')
    myotherin = Float(3, iotype='in')

    def configure(self):
        super(Comp2, self).configure()
        self.add('comp', Comp())
        self.driver.workflow.add('comp')

    def execute(self):
        self.comp.myin = self.myin2
        self.comp.myotherin = self.myotherin
        super(Comp2, self).execute()
        self.myout2 = self.comp.myout


class FrameworkAssembly(Assembly):
    """ Run two connected sub-assemblies (framework 'components'). """

    def configure(self):
        super(FrameworkAssembly, self).configure()

        self.add('c1', Comp1())
        self.add('c2', Comp2())
        self.driver.workflow.add(['c1', 'c2'])

        self.connect('c1.myout1', 'c2.myin2')

        self.create_passthrough('c1.myin1')
        self.create_passthrough('c1.myotherin')
        self.create_passthrough('c1.myout1')
        self.create_passthrough('c2.myout2')


class FrameworkStudy(Assembly):
    """ Use DOEdriver to run a study. """

    def configure(self):
        """ Configure driver and its workflow. """
        super(FrameworkStudy, self).configure()
        self.add('framework', FrameworkAssembly()) 

        driver = self.add('driver', DOEdriver())
        self.driver.DOEgenerator = FullFactorial(3)

        driver.workflow.add('framework')

        driver.add_parameter('framework.myin1', low=-10.0, high = 10.0)
        driver.add_parameter('framework.myotherin', low=-10.0, high=10.0)
        driver.case_outputs = ['framework.myout2']


class TestCase(unittest.TestCase):
    """ Derived from NREL bug, where c2 executed only once. """

    def test_subassembly(self):
        top = set_as_top(FrameworkStudy())
        top.run()
        self.assertEqual(top.framework.c1.exec_count, 9)
        self.assertEqual(top.framework.c2.exec_count, 9)


if __name__ == '__main__':
    unittest.main()

