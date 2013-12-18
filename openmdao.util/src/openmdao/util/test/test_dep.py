"""
Test Dependency Functions
"""

import os
import unittest
from nose import SkipTest

from openmdao.util.dep import PythonSourceTreeAnalyser

class DepTestCase(unittest.TestCase):

    def setUp(self):
        try:
            # on windows, if we just do 'import openmdao.main and import openmdao.lib', 
            # we get errors like 'module has no attribute "main".  Importing specific'
            # files within main and lib fixes it.
            from openmdao.main import component
            from openmdao.lib import releaseinfo
        except ImportError:
            # don't perform this test if openmdao.main 
            # and openmdao.lib aren't present
            raise SkipTest("this test requires openmdao.main and openmdao.lib")
        self.startdirs = [os.path.dirname(component.__file__), 
                          os.path.dirname(releaseinfo.__file__)]
        
    def test_PythonSourceTreeAnalyser(self):
       
        skipdirs = set(['test', 'docs', 'examples', 'optproblems',
                        'build', 'dist'])

        psta = PythonSourceTreeAnalyser(self.startdirs, 
                                        direxclude=lambda d: d in skipdirs)
        
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
        comps.extend(psta.find_inheritors('traits.api.Array'))
        comps = [x.rsplit('.',1)[1] for x in comps] 
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
