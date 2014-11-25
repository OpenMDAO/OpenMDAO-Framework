# pylint: disable-msg=C0111,C0103

import unittest
import numpy as np

from openmdao.main.api import Assembly, set_as_top, Component
from openmdao.lib.geometry.geom_data import GeomData
from openmdao.main.datatypes.vtree import VarTree

class GeomDataTestCase(unittest.TestCase):
    
    def test_basic(self):
        
        top = set_as_top(Assembly())
        box = top.add('box', Component())
        box.add('geo', VarTree(GeomData(13, 17, 3), iotype='in'))
        
        self.assertTrue(box.geo.points.shape == (13, 3))
        self.assertTrue(box.geo.facets.shape == (17, 3))
        self.assertTrue(issubclass(box.geo.facets.dtype.type,np.int))

        box.add('geo2', VarTree(GeomData(13, 17, 4), iotype='out'))
        self.assertTrue(box.geo2.facets.shape == (17, 4))
       
        try:
            box.geo = GeomData(3, 4, 5)
        except ValueError, err:
            msg = 'facet size must be either 3 or 4'
            self.assertEqual(str(err), msg)
        else:
            raise RuntimeError("ValuError expected")
        
if __name__ == "__main__":
    unittest.main()