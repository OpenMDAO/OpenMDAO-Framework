
# pylint: disable-msg=C0111
import unittest

from openmdao.main.api import Container

class HierarchyTestCase(unittest.TestCase):

    def setUp(self):
        self.top = Container()
        self.h1 = Container()
        self.top.add('h1', self.h1)
        self.h11 = Container()    
        self.h12 = Container()
        self.h121 = Container()
        self.h122 = Container()
        self.h1.add('h11',self.h11)
        self.h1.add('h12',self.h12)
        self.h12.add('h121',self.h121)
        self.h12.add('h122',self.h122)
    
    def test_pathnames(self):
        self.assertEqual(self.h1.get_pathname(), 'h1')
        self.assertEqual(self.h11.get_pathname(), 'h1.h11')
        self.assertEqual(self.h12.get_pathname(), 'h1.h12')   
        self.assertEqual(self.h121.get_pathname(), 'h1.h12.h121')
        self.assertEqual(self.h122.get_pathname(), 'h1.h12.h122')
    
    def test_error_handling(self):
        try:
            self.h121.raise_exception("bad value", ValueError)
        except ValueError, err:
            self.assertEqual(str(err), "h1.h12.h121: bad value")
        else:
            self.fail('ValueError expected')
            
        self.h121._logger.error("can't start server")
        self.h121._logger.warning("I wouldn't recommend that")
        self.h121._logger.info("fyi")
        self.h121._logger.debug("dump value = 3")
    
    
if __name__ == "__main__":
    unittest.main()
