"""
Test Dependency Functions
"""

import os, sys
import unittest
from nose import SkipTest

from openmdao.util.dep import PythonSourceFileAnalyser, PythonSourceTreeAnalyser

class DepTestCase(unittest.TestCase):

    def test_PythonSourceTreeAnalyser(self):
        try:
            import openmdao.main
            import openmdao.lib
        except ImportError:
            # don't perform this test if openmdao.main 
            # and openmdao.lib aren't present
            raise SkipTest("this test requires openmdao.main and openmdao.lib")
        startdirs = [os.path.dirname(openmdao.main.__file__), 
                     os.path.dirname(openmdao.lib.__file__)]
        psta = PythonSourceTreeAnalyser(startdirs, os.path.join('*','test','*'))
        
        self.assertTrue('openmdao.main.component.Component' in 
                        psta.graph['openmdao.main.container.Container'])
        self.assertTrue('openmdao.main.assembly.Assembly' in 
                        psta.graph['openmdao.main.component.Component'])
        
        self.assertTrue('openmdao.lib.datatypes.float.Float' in
                        psta.graph['openmdao.main.variable.Variable'])
        
        comps = psta.find_inheritors('openmdao.main.component.Component')
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
            ])
        cset = cset - excludes
        
        from openmdao.main.api import get_available_types
        from openmdao.main.factorymanager import _container_groups
        groups = [ 'openmdao.component',
                   'openmdao.driver',
                   'openmdao.variable',
                   'openmdao.surrogatemodel',
                   'openmdao.differentiator']
        types = set([x[0] for x in get_available_types(groups)])
        types = [x.rsplit('.',1)[1] for x in types if x.startswith('openmdao.')]
        
        tset = set(types)
        noentrypts = cset-tset
        if noentrypts:
            self.fail("the following Components are not registered using entry points: %s" % noentrypts)
    
if __name__ == '__main__':
    unittest.main()
