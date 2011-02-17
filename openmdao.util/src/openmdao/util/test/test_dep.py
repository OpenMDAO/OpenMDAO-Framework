"""
Test Dependency Functions
"""

import os, sys
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
        
        self.assertTrue('openmdao.main.component.Component' in 
                        psta.graph['openmdao.main.container.Container'])
        self.assertTrue('openmdao.main.assembly.Assembly' in 
                        psta.graph['openmdao.main.component.Component'])
        
        self.assertTrue('openmdao.lib.datatypes.float.Float' in
                        psta.graph['enthought.traits.api.TraitType'])
        
        comps = psta.find_inheritors('openmdao.main.component.Component')
        comps.extend(psta.find_inheritors('enthought.traits.api.TraitType'))
        comps.extend(psta.find_inheritors('enthought.traits.api.Array'))
        comps = [x.rsplit('.',1)[1] for x in comps]
        comps.remove('Driver')
        comps.remove('CaseIterDriverBase')
        comps.remove('PassthroughTrait')
        
        from openmdao.main.api import get_available_types
        groups = [ 'openmdao.component',
                   'openmdao.driver',
                   'openmdao.variable']
        types = set([x[0] for x in get_available_types(groups)])
        types = [x.rsplit('.',1)[1] for x in types]
        
        noentrypts = set(comps)-set(types)
        if noentrypts:
            self.fail("the following Components are not registered using entry points: %s" % noentrypts)
        
    
if __name__ == '__main__':
    unittest.main()
    #import ast
    #import parser
    #from openmdao.util.fileutil import find_files
    
    #passcount = 0
    #fails = {}
    #for pyfile in find_files('*.py', sys.argv[1]):
        #f = open(pyfile, 'Ur')
        #content = f.read()+'\n'
        #f.close()
        #try:
            #ast.parse(content, pyfile)
            #passcount += 1
            ##print 'GOOD: %s' % os.path.basename(pyfile)
        #except Exception as err:
            #print 'FAIL: %s' % pyfile
            #print '**** %s' % str(err)
            #fails[pyfile] = str(err)
            
    #print '%s passed' % passcount
    #print '%s failed' % len(fails)

