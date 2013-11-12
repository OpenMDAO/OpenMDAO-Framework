import unittest

from openmdao.main.datatypes.api import Int, List

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
        
    def test_change_n_mux(self): 
        mux = Mux(2)
        
        mux.input_1 = 1.5
        mux.input_2 = "a"
        
        mux.n = 3
        #check that old traits were removed 
        self.assertEqual(mux.input_1,None)
        self.assertEqual(mux.input_2,None)
        self.assertEqual(mux.input_3,None)
        
    def test_change_n_demux(self): 
        demux = DeMux(2)
        
        demux.inputs = [1,"a"]
        
        demux.n = 3
        
        self.assertEqual(demux.output_1,None)
        self.assertEqual(demux.output_2,None)
        self.assertEqual(demux.output_3,None)
    
    
if __name__=="__main__": 
    unittest.main()