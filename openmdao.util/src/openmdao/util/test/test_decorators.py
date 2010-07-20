
import unittest

from openmdao.util.decorators import add_delegate


class GoodDelegate(object):
    cls_y = 4
    
    def __init__(self):
        self.inst_y = 3
        
    def del_amethod(self):
        return self.inst_y
    
    def _priv_method(self):
        pass
    
class BadDelegate1(object):
    cls_x = 4
    
    def __init__(self):
        self.inst_y = 3
        
    def del_amethod(self):
        return self.inst_y
    
    def _priv_method(self):
        pass
    
class BadDelegate2(object):
    cls_y = 4
    
    def __init__(self):
        self.inst_x = 3
        
    def del_amethod(self):
        return self.inst_y
    
    def _priv_method(self):
        pass


class BadDelegate3(object):
    cls_y = 4
    
    def __init__(self):
        self.inst_y = 3
        
    def amethod(self):
        return self.inst_y
    
    def _priv_method(self):
        pass
    


    
    
class decoratorTestCase(unittest.TestCase):
    """ Test namelist writer functions. """

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_add_delegate(self):
        @add_delegate({'gooddel': GoodDelegate})
        class Foo(object):
            cls_x = 1
            
            def __init__(self):
                self.inst_x = 9
                
            def amethod(self, a, b, c='foo'):
                pass
            
            def genmethod(self, *args, **kwargs):
                pass
            
            def _priv_method(self):
                pass
    


if __name__ == '__main__':
    unittest.main()
    