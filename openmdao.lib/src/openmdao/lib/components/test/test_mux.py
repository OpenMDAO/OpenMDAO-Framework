import unittest

from openmdao.lib.datatypes.api import Int, List

from openmdao.lib.components.mux import Mux,DeMux
    

class testMux(unittest.TestCase): 

    
    def test_2mux(self): 
        mux = Mux(2)
        
        mux.input_1 = 1.5
        mux.input_2 = "a"
        mux.execute()
        
        self.assertEqual(mux.output,[1.5,"a"])
              
    def test_2demux(self):   
        demux = DeMux(2)
        
        demux.inputs = [1,"a"]
        demux.execute()
        
        self.assertEqual(demux.output_1,1)
        self.assertEqual(demux.output_2,"a")
    
    
if __name__=="__main__": 
    unittest.main()