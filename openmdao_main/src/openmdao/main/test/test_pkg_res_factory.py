"""
pkg_res_factory test
"""

import logging
import os
import unittest

# pylint: disable-msg=F0401
from pkg_resources import DistributionNotFound, VersionConflict
from pkg_resources import Requirement, Environment, working_set

from openmdao.main.pkg_res_factory import PkgResourcesFactory
from openmdao.main.api import Component, get_available_types


# pylint: disable-msg=C0103
class PkgResFactoryTestCase(unittest.TestCase):
    """tester for pkg_res_factory"""

    def test_load(self):
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

    def test_get_available_types(self):
        tups = get_available_types()
        types = set([x[0] for x in tups])
        iface_dict = dict((key, value['ifaces']) for (key, value) in tups)
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
                        'openmdao.lib.drivers.iterate.FixedPointIterator',
                        'openmdao.lib.optproblems.sellar.SellarProblem',
                        'openmdao.lib.optproblems.branin.BraninProblem',
                        'openmdao.lib.optproblems.polyscale.PolyScalableProblem'])
        missing = expected - types
        if missing:
            self.fail("the following expected types were missing: %s" % missing)

        for typ,meta in tups:
            if not isinstance(meta, dict):
                self.fail("%s did not return a metadata dict from get_available_types" % typ)
            if 'version' not in meta:
                self.fail("the metadata for %s did not contain 'version'" % typ)
            if 'ifaces' not in meta:
                self.fail("the metadata for %s did not contain 'ifaces'" % typ)

        self.assertEqual(iface_dict['openmdao.lib.drivers.conmindriver.CONMINdriver'],
                         ['IHasObjective', 'IComponent', 'IHasParameters', 'IHasIneqConstraints', 'IContainer', 'IDriver', 'IOptimizer'])
if __name__ == "__main__":
    unittest.main()


