
# pylint: disable-msg=C0111
import unittest

from enthought.traits.api import TraitError

from openmdao.main.api import Container

class HierarchyTestCase(unittest.TestCase):

    def setUp(self):
        self.top = Container()
        self.h1 = Container(doc="a hierarchy member")
        self.top.add_container('h1', self.h1)
        self.h11 = Container()    
        self.h12 = Container()
        self.h121 = Container()
        self.h122 = Container()
        self.h1.add_container('h11',self.h11)
        self.h1.add_container('h12',self.h12)
        self.h12.add_container('h121',self.h121)
        self.h12.add_container('h122',self.h122)
    
    def test_pathnames(self):
        self.assertEqual(self.h1.get_pathname(), 'h1')
        self.assertEqual(self.h11.get_pathname(), 'h1.h11')
        self.assertEqual(self.h12.get_pathname(), 'h1.h12')   
        self.assertEqual(self.h121.get_pathname(), 'h1.h12.h121')
        self.assertEqual(self.h122.get_pathname(), 'h1.h12.h122')
    
    def test_doc(self):
        self.assertEqual(self.h1.__doc__ , "a hierarchy member")
    
    #def test_names(self):
        #for name in ['8foobar', 'abc def', 'abc*', 'abc!xx', 'a+b', '0', ' blah', 'blah '
                     #'.', 'abc.', 'abc..xyz']:
            #try:
                #h1 = Container(name)
            #except TraitError, err:
                #self.assertEqual(str(err), 
                                 #"name '%s' contains illegal characters" %
                                 #name)
            #else:
                #self.fail("TraitError expected for '%s'" % name)
    
    #def test_rename(self):
        #obj = Container('valid', None)
        #self.assertEqual(obj.name, 'valid')
        #obj.rename('valid2')
        #self.assertEqual(obj.name, 'valid2')

        #try:
            #obj.rename('')
        #except NameError, err:
            #self.assertEqual(str(err), 'valid2: name must be non-null')
        #else:
            #self.fail('Expected NameError')

        #try:
            #obj.rename('8foobar')
        #except NameError, err:
            #msg = "valid2: name '8foobar' contains illegal characters"
            #self.assertEqual(str(err), msg)
        #else:
            #self.fail('Expected NameError')
        
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
