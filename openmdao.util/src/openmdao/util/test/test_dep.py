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
        # find all inheritors from openmdao.main.api.Component
        #for edge in graph.in_edges('openmdao.main.component.Component'):
            #print edge

    
if __name__ == '__main__':
    unittest.main()

