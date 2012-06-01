"""
Test Dependency Functions
"""

import os, sys
import unittest
from nose import SkipTest

from openmdao.util.dep import PythonSourceFileAnalyser, PythonSourceTreeAnalyser

class DepTestCase(unittest.TestCase):

    def setUp(self):
        try:
            import openmdao.main
            import openmdao.lib
        except ImportError:
            # don't perform this test if openmdao.main 
            # and openmdao.lib aren't present
            raise SkipTest("this test requires openmdao.main and openmdao.lib")
        self.startdirs = [os.path.dirname(openmdao.main.__file__), 
                          os.path.dirname(openmdao.lib.__file__)]
        
    def test_PythonSourceTreeAnalyser(self):
        
        def exclude_tests(pname):
            parts = pname.split(os.sep)
            return 'test' in parts
        
        psta = PythonSourceTreeAnalyser(self.startdirs, exclude_tests)
        
        self.assertTrue('openmdao.main.component.Component' in 
                        psta.graph['openmdao.main.container.Container'])
        self.assertTrue('openmdao.main.assembly.Assembly' in 
                        psta.graph['openmdao.main.component.Component'])
        
        self.assertTrue('openmdao.main.datatypes.float.Float' in
                        psta.graph['openmdao.main.variable.Variable'])
        
        comps = psta.find_inheritors('openmdao.main.component.Component')
        icomps = psta.find_inheritors('IComponent')
        
        self.assertTrue('openmdao.main.assembly.Assembly' in icomps)
        
        comps.extend(psta.find_inheritors('openmdao.main.variable.Variable'))
        comps.extend(psta.find_inheritors('enthought.traits.api.Array'))
        comps = [x.rsplit('.',1)[1] for x in comps if '.examples.' not in x and '.optproblems.' not in x]
        cset = set(comps)
        excludes = set([
            'Driver',
            'DriverUsesDerivatives',
            'DistributionCaseDriver',
            'CaseIterDriverBase',
            'PassthroughTrait',
            'PassthroughProperty',
            'OptProblem',
            'TraitArray',
            'Broadcast', # utility class for bliss2000
            'SubSystemOpt', # utility class for bliss2000
            'SubSystemObj' # utility class for bliss2000
            ])
        cset = cset - excludes
        
        from openmdao.main.api import get_available_types
        types = set([x[0] for x in get_available_types()])
        types = [x.rsplit('.',1)[1] for x in types if x.startswith('openmdao.')]
        
        tset = set(types)
        noentrypts = cset-tset
        if noentrypts:
            self.fail("the following Components are not registered using entry points: %s" % noentrypts)
    
if __name__ == '__main__':
    unittest.main()
