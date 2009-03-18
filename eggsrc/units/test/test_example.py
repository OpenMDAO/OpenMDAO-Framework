import unittest

class test_import(unittest.TestCase):
    def test_exception(self):
    
        x = 1
        y = 2
        
        try: 
            x+y
        except ValueError,err:
            self.assertEqual(str(err),"some crappy error message")
        except:
            self.fail("expecting ValueError")
        
        
if __name__ == "__main__":
    unittest.main()       
    
    
