
# pylint: disable-msg=C0111
import unittest

from openmdao.main import HierarchyMember

class HierarchyTestCase(unittest.TestCase):

    def setUp(self):
        self.h1 = HierarchyMember('h1', None, doc="a hierarchy member")
        self.h11 = HierarchyMember('h11', self.h1)    
        self.h12 = HierarchyMember('h12', self.h1)
        self.h121 = HierarchyMember('h121', self.h12)
        self.h122 = HierarchyMember('h122', self.h12)
    
    def test_pathnames(self):
        self.assertEqual(self.h1.get_pathname(), 'h1')
        self.assertEqual(self.h11.get_pathname(), 'h1.h11')
        self.assertEqual(self.h12.get_pathname(), 'h1.h12')   
        self.assertEqual(self.h121.get_pathname(), 'h1.h12.h121')
        self.assertEqual(self.h122.get_pathname(), 'h1.h12.h122')
    
    def test_doc(self):
        self.assertEqual(self.h1.__doc__ , "a hierarchy member")
    
    def test_names(self):
        for name in ['8foobar', 'abc def', 'abc*', 'abc!xx', 'a+b', '0', ' blah', 'blah '
                     '.', 'abc.', 'abc..xyz']:
            try:
                h1 = HierarchyMember(name)
            except NameError, err:
                self.assertEqual(str(err), 
                                 "%s: name '%s' contains illegal characters" % 
                                 (name,name))
            else:
                self.fail("NameError expected for '%s'" % name)
    
        
    def test_error_handling(self):
        try:
            self.h121.raise_exception("bad value", ValueError)
        except ValueError, err:
            self.assertEqual(str(err), "h1.h12.h121: bad value")
        else:
            self.fail('ValueError expected')
            
        self.h121.error("can't start server")
        self.h121.warning("I wouldn't recommend that")
        self.h121.info("fyi")
        self.h121.debug("dump value = 3")
    
    
if __name__ == "__main__":
    unittest.main()
