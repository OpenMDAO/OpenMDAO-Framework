"""
pkg_res_factory test
"""

import logging
import os
import unittest

# pylint: disable-msg=F0401
from pkg_resources import DistributionNotFound, VersionConflict
from pkg_resources import Requirement, Environment, working_set

from openmdao.main.pkg_res_factory import import_version, PkgResourcesFactory
from openmdao.main.api import Component, get_available_types


class CoordComp(Component):
    pass


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
            import_version('bogus', Requirement.parse('bogus==1.0'),
                                     Environment(['plugins']))
        except DistributionNotFound, err:
            self.assertEqual(str(err),
                            'could not find distribution satisfying bogus==1.0')
        
    def test_load(self):
        """
        Verify that a plugin can be loaded successfully through a pkg_resources
        entry point.
        """
        logging.debug('')
        logging.debug('test_load')

        # make sure we're looking in the right spot for the plugins whether
        # we're in a develop egg or in the released version
        dist = working_set.find(Requirement.parse('openmdao.test'))
        fact = PkgResourcesFactory(['openmdao.component'],
                                   [os.path.join(dist.location,
                                                 'openmdao',
                                                 'test','plugins')],
                                   )
        
        comp = fact.create('testplugins.components.dumb.DumbComponent')
        logging.debug('    loaders:')
        for key, value in fact._loaders.items():
            logging.debug('        %s:', key)
            for val in value:
                logging.debug('                name: %s', val.name)
                logging.debug('               group: %s', val.group)
                logging.debug('                dist: %s', val.dist)
                logging.debug('            entry_pt: %s', val.entry_pt)
                logging.debug('                ctor: %s', val.ctor)
                logging.debug('')

        self.assertEqual(comp.svar,'abcdefg')
        comp.run()
        self.assertEqual(comp.svar,'gfedcba')
        
    def test_load2(self):
        # make sure we're looking in the right spot for the plugins whether
        # we're in a develop egg or in the released version
        dist = working_set.find(Requirement.parse('openmdao.test'))
        fact = PkgResourcesFactory(['openmdao.component'], None)
        
        comp = fact.create('openmdao.test.execcomp.ExecComp', 
                           exprs=['x = a+1','y=b-2','z=x*2'])
        comp.a = 4
        comp.b = 2
        comp.run()
        self.assertEqual(comp.x, 5)
        self.assertEqual(comp.y, 0)
        self.assertEqual(comp.z, 10)
        
        
    def test_load_version(self):
        """load a specific version, then try to load a conflicting version"""
        
        dist = working_set.find(Requirement.parse('openmdao.test'))
        fact = PkgResourcesFactory(['openmdao.dumbplugins'],
                                   [os.path.join(dist.location,
                                                 'openmdao','test',
                                                 'plugins')])
        foo = fact.create('foo.Comp1Plugin', version='1.0')
        self.assertEqual(foo.version, '1.0')
        
        # now try to create an object that requires a conflicting version of foo
        self.assertRaises(VersionConflict,
                          fact.create,'foo.Comp1Plugin',
                          version='1.4')
        
        # now request a non-existent version of foo
        foo10 = fact.create('foo.Comp1Plugin', version='10.5')
        self.assertEqual(foo10, None)
        
    def test_get_loaders(self):
        """test retrieval of loaders"""
        # Get a list of entry point loaders for the openmdao.dumbplugins 
        # group.       
        dist = working_set.find(Requirement.parse('openmdao.test'))
        fact = PkgResourcesFactory(['openmdao.dumbplugins'],
                                   [os.path.join(dist.location,
                                                 'openmdao',
                                                 'test','plugins')])
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
        mybar = dumb_loaders[0].create(None)
        self.assertEqual(mybar.version, '1.0')
        
    def test_get_available_types(self):
        types = set([x[0] for x in get_available_types()])
        expected = set(['openmdao.lib.components.external_code.ExternalCode',
                        'openmdao.lib.components.mux.DeMux',
                        'openmdao.lib.drivers.doedriver.DOEdriver',
                        'openmdao.lib.drivers.genetic.Genetic',
                        'openmdao.lib.drivers.caseiterdriver.CaseIteratorDriver',
                        'openmdao.lib.components.metamodel.MetaModel',
                        'openmdao.lib.components.expected_improvement_multiobj.MultiObjExpectedImprovement',
                        'openmdao.lib.drivers.conmindriver.CONMINdriver',
                        'openmdao.lib.drivers.simplecid.SimpleCaseIterDriver',
                        'openmdao.lib.components.pareto_filter.ParetoFilter',
                        'openmdao.lib.drivers.newsumtdriver.NEWSUMTdriver',
                        'openmdao.lib.components.mux.Mux',
                        'openmdao.lib.components.expected_improvement.ExpectedImprovement',
                        'openmdao.test.execcomp.ExecComp',
                        'openmdao.main.assembly.Assembly',
                        'openmdao.lib.drivers.iterate.FixedPointIterator',])
        missing = expected - types
        if missing:
            self.fail("the following expected types were missing: %s" % missing)
        
if __name__ == "__main__":
    unittest.main()


