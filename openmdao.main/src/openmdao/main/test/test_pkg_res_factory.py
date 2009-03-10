"""
pkg_res_factory test
"""

import os
import unittest

# pylint: disable-msg=F0401
from pkg_resources import DistributionNotFound, VersionConflict
from pkg_resources import Requirement, Environment, working_set

import openmdao.main.pkg_res_factory as prfactory


# pylint: disable-msg=C0103
class PkgResFactoryTestCase(unittest.TestCase):
    """tester for pkg_res_factory"""

    def setUp(self):
        """this setup function will be called before each test"""
        pass        

    def tearDown(self):
        """this teardown function will be called after each test"""
        pass
    
    def test_import_not_found(self):
        """try importing a distrib that doesn't exist"""
        try:
            prfactory.import_version(Requirement.parse('bogus==1.0'),
                                     Environment(['plugins']))
        except DistributionNotFound, err:
            self.assertEqual(str(err),
                            'could not find distribution satisfying bogus==1.0')
        
    def test_load(self):
        """
        Verify that a plugin can be loaded successfully through a pkg_resources
        entry point.
        """
        # make sure we're looking in the right spot for the plugins whether
        # we're in a develop egg or in the released version
        dist = working_set.find(Requirement.parse('openmdao.main'))
        fact = prfactory.PkgResourcesFactory([os.path.join(dist.location,
                                                           'openmdao',
                                                           'main','plugins')],
                                             ['openmdao.components'])
        
        comp = fact.create('testplugins.components.dumb.DumbComponent','foo')
        self.assertEqual(comp.svar,'abcdefg')
        comp.run()
        self.assertEqual(comp.svar,'gfedcba')
        
    def test_load_version(self):
        """load a specific version, then try to load a conflicting version"""
        
        dist = working_set.find(Requirement.parse('openmdao.main'))
        fact = prfactory.PkgResourcesFactory([os.path.join(dist.location,
                                                           'openmdao','main',
                                                           'plugins')],
                                             ['openmdao.dumbplugins'])
        foo = fact.create('foo.Comp1Plugin', name='foo', version='1.0')
        self.assertEqual(foo.version, '1.0')
        
        # now try to create an object that requires a conflicting version of foo
        self.assertRaises(VersionConflict,
                          fact.create, 'foo.Comp1Plugin', name='foo2',
                          version='1.4')
        
        # now request a non-existent version of foo
        foo10 = fact.create('foo.Comp1Plugin', name='foo2', version='10.5')
        self.assertEqual(foo10, None)
        
    def test_get_loaders(self):
        """test retrieval of loaders"""
        # Get a list of entry point loaders for the openmdao.dumbplugins 
        # group.       
        dist = working_set.find(Requirement.parse('openmdao.main'))
        fact = prfactory.PkgResourcesFactory([os.path.join(dist.location,
                                                           'openmdao',
                                                           'main', 'plugins')],
                                             ['openmdao.dumbplugins'])
        # first, look for active loaders. list should be empty
        dumb_loaders = fact.get_loaders('openmdao.dumbplugins')
        self.assertEqual(len(dumb_loaders), 0)
        
        # now, get all of the loaders, including inactive ones
        dumb_loaders = fact.get_loaders('openmdao.dumbplugins', active=False)
        self.assertEqual(len(dumb_loaders), 6)
        self.assertEqual(dumb_loaders[0].name, 'bar.Comp1Plugin')
        self.assertEqual(dumb_loaders[0].dist.version, '1.0')
        self.assertEqual(dumb_loaders[0].dist.project_name, 'bar')
        self.assertEqual(dumb_loaders[0].ctor, None)

        # now, create a plugin object, which will make its loader active
        comp = fact.create('bar.Comp1Plugin')
        self.assertEqual(comp.version, '1.0')
        
        # now there should be one active loader (its ctor should not be None)
        dumb_loaders = fact.get_loaders('openmdao.dumbplugins')
        self.assertEqual(len(dumb_loaders), 1)
        self.assertEqual(dumb_loaders[0].name, 'bar.Comp1Plugin')
        self.assertEqual(dumb_loaders[0].dist.version, '1.0')
        self.assertEqual(dumb_loaders[0].dist.project_name, 'bar')
        mybar = dumb_loaders[0].create(None, 'mybar')
        self.assertEqual(mybar.version, '1.0')
        
        
if __name__ == "__main__":
    unittest.main()

