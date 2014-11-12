import unittest

from openmdao.main.api import VariableTree, Component, Assembly, set_as_top
from openmdao.lib.datatypes.api import VarTree, Float, List

class RegionVT(VariableTree):

    c0 = Float()
    c1 = Float()

class SectionVT(VariableTree):

    s0 = Float()
    s1 = Float()
    regions = List()

    def add_region(self, name):

        self.add(name, VarTree(RegionVT()))
        self.regions.append(name)

class BladeVT(VariableTree):

    def add_section(self, name):

        self.add(name, VarTree(SectionVT()))

class SplinedRegion(Component):

    region = VarTree(RegionVT(), iotype='out')

    def execute(self):
        #print 'computing splines for c0 and c1'
        self.region.c0 = 1.0 #np.sin(np.linspace(0,1,10))
        self.region.c1 = 2.0 #np.cos(np.linspace(0,1,10))

class BladeSection(Assembly):

    section = VarTree(SectionVT(), iotype='out')

    def __init__(self, nr):
        super(BladeSection, self).__init__()

        for i in range(nr):
            name = 'region%02d' % i
            #print 'adding region ', name
            self.add(name, SplinedRegion())
            self.driver.workflow.add(name)
            self.section.add_region(name)

            self.connect(name + '.region', 'section.' + name)

class BladeStructure(Assembly):

    st3d = VarTree(BladeVT(), iotype='out')

    def configure(self):

        self.add('section0', BladeSection(5))
        self.driver.workflow.add('section0')
        self.st3d.add_section('section0')
        
        # Copy vartree before connecting to make sure it matches.
        self.st3d.section0 = self.section0.section.copy()
        
        self.connect('section0.section', 'st3d.section0')

class Builder(Component):

    st3d = VarTree(BladeVT(), iotype='in')

    def execute(self):
        for region in self.st3d.section0.regions:
            #print region, getattr(self.st3d.section0, region)
            getattr(self.st3d.section0, region)

class Blade(Assembly):

    def configure(self):

        self.add('blade_st', BladeStructure())
        self.add('builder', Builder())

        self.driver.workflow.add(['blade_st','builder'])
        
        # Copy vartree before connecting to make sure it matches.
        self.builder.st3d = self.blade_st.st3d.copy()
        
        self.connect('blade_st.st3d', 'builder.st3d')

class VTreeCopyTestCase(unittest.TestCase):
    def test_copy(self):
        top = set_as_top(Blade())
        top.run()  # this raised an Exception when the bug was present

if __name__ == '__main__':
    unittest.main()
