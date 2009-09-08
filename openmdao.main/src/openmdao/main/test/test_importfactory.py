
import unittest

# pylint: disable-msg=C0111

from openmdao.main.api import ImportFactory

class ImportFactoryTestCase(unittest.TestCase):
    
    def test_import(self):
        ifactory = ImportFactory()
        mycomp = ifactory.create('openmdao.main.component.Component')
        self.assertEqual(mycomp.__class__.__name__ , 'Component')
    
    def test_bad_import(self):
        ifactory = ImportFactory()
        obj = ifactory.create('boguscompname')
        self.assertEqual(obj, None)
    
    def test_version_import(self):
        ifactory = ImportFactory()
        obj = ifactory.create('openmdao.main.component.Component', version='1.2')
        self.assertEqual(obj, None)
    
    def test_server_import(self):
        ifactory = ImportFactory()
        obj = ifactory.create('openmdao.main.component.Component', 
                              server='open_mdao_srv')
        self.assertEqual(obj, None)
    
    def test_res_desc_import(self):
        ifactory = ImportFactory()
        obj = ifactory.create('openmdao.main.component.Component', 
                              res_desc={'my_attribute':4})
        self.assertEqual(obj, None)


if __name__ == "__main__":
    unittest.main()

