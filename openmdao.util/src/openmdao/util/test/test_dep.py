"""
Test Dependency Functions
"""

import os
import unittest

from openmdao.util.dep import PythonSourceFileAnalyser, PythonSourceTreeAnalyser

class DepTestCase(unittest.TestCase):

    def test_PythonSourceTreeAnalyser(self):
        try:
            import openmdao.main
            import openmdao.lib
        except ImportError:
            return  # don't perform this test if openmdao.main 
                    # and openmdao.lib aren't present
        excludes = [os.path.join('*','test','*')]
        startdirs = [os.path.dirname(openmdao.main.__file__), 
                     os.path.dirname(openmdao.lib.__file__)]
        psta = PythonSourceTreeAnalyser(startdirs, excludes)
        psta.analyze()
        
        self.assertTrue('openmdao.main.component.Component' in 
                        psta.graph['openmdao.main.container.Container'])
        self.assertTrue('openmdao.main.assembly.Assembly' in 
                        psta.graph['openmdao.main.component.Component'])
        
        comps = psta.find_inheritors('openmdao.main.component.Component')
        comps = [x.rsplit('.',1)[1] for x in comps]
        comps.remove('Driver')
        
        from openmdao.main.api import get_available_types
        types = set([x[0] for x in get_available_types(['openmdao.component'])])
        types = [x.rsplit('.',1)[1] for x in types]
        
        self.assertEqual(set(comps), set(types))
        
    
if __name__ == '__main__':
    unittest.main()

