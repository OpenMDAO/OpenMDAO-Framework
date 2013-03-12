# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Assembly, Component
from openmdao.main.datatypes.api import Geom
from openmdao.main.interfaces import implements, IStaticGeometry


class MyGeo(object):
    
    implements(IStaticGeometry)
    
    def get_tessellation(self):
        """Usually tesselate"""
        
        return 1

class GeoSource(Component):
    
    g_out = Geom(iotype='out')
    
    def execute(self):
        """produce geo"""
        self.g_out = MyGeo()
        
class GeoSink(Component):
        
    g_inp = Geom(iotype='in')
    g_extra = Geom(iotype='in')
    
    def execute(self):
        """test geo"""
        pass
    
class GeomTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.top = Assembly()
        self.top.add('g1', GeoSource())
        self.top.add('g2', GeoSink())
        self.top.connect('g1.g_out', 'g2.g_inp')
        self.top.driver.workflow.add(['g1', 'g2'])
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.top = None
    
    def testGeom(self):
        
        self.top.run()
        
        self.assertEqual(1, self.top.g2.g_inp.get_tessellation())
        
        try:
            self.top.g2.g_extra = "hey"
        except TypeError as err:
            msg = "g2 (1-2): g_extra must provide interface 'IStaticGeometry'"
            self.assertEqual(str(err), msg)        
        else:
            self.fail("exception expected")
        
if __name__ == "__main__":
    unittest.main()    
