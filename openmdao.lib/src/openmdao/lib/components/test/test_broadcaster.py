import unittest
from openmdao.main.api import Assembly, Component
from openmdao.lib.components.broadcaster import Broadcaster
from openmdao.lib.datatypes.api import Float,Str


class Dummy(Component): 
    
    x = Float(iotype="out")
    y = Float(iotype="in")    
        
        
class testBroadcaster(unittest.TestCase): 
    
    def test_create(self): 
        b1 = Broadcaster(['x','y','z'])

        self.assertTrue(hasattr(b1,"x_in"))
        self.assertTrue(hasattr(b1,"x"))
        self.assertTrue(hasattr(b1,"y_in"))
        self.assertTrue(hasattr(b1,"y"))
        self.assertTrue(hasattr(b1,"z_in"))
        self.assertTrue(hasattr(b1,"z"))
        
        b1.names = ['a','b']
        
        self.assertFalse(hasattr(b1,"x_in"))
        self.assertFalse(hasattr(b1,"x"))
        self.assertFalse(hasattr(b1,"y_in"))
        self.assertFalse(hasattr(b1,"y"))
        self.assertFalse(hasattr(b1,"z_in"))
        self.assertFalse(hasattr(b1,"z"))
        
        self.assertTrue(hasattr(b1,'a_in'))
        self.assertTrue(hasattr(b1,'a'))
        self.assertTrue(hasattr(b1,'b_in'))
        self.assertTrue(hasattr(b1,'b'))
        
        b1.types = {'a':Float,'default':Float}
        self.assertTrue(hasattr(b1,'a_in'))
        self.assertTrue(hasattr(b1,'a'))
        self.assertTrue(hasattr(b1,'b_in'))
        self.assertTrue(hasattr(b1,'b'))
        
        
    def test_execute(self): 
        b1 = Broadcaster(['x','y'])
        
        b1.x_in = 2
        b1.y_in = 1
        
        b1.run()
        
        self.assertEqual(b1.x,2)
        self.assertEqual(b1.y,1)
        
        
    def test_connections(self): 
        
        asm = Assembly()
        asm.add('dummy1',Dummy())
        asm.add('dummy2',Dummy())
        asm.add('bcast',Broadcaster(['x']))
        
        asm.connect('dummy1.x','bcast.x_in')
        asm.connect('bcast.x','dummy2.y')
        
        self.assertEqual(set(asm.list_connections()),set([('dummy1.x', 'bcast.x_in'), ('bcast.x', 'dummy2.y')]))
        asm.bcast.names = ['z']
        
        self.assertEqual(asm.list_connections(),[])    
        
        
    def test_error(self):    
        try: 
            b = Broadcaster(['x'],{'y':Float})
        except ValueError, err: 
            self.assertEqual(str(err),': No type was provided for "x" and no "default" type was provided. '
                'Specify at least one of these')
        else: 
            self.fail('ValueError Expected')
            
        
        
            
            
        
        
        