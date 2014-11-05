"""
    nozzle_geometry_doe.py - Top level assembly for nozzle geometry doe problem
"""

from openmdao.main.api import Assembly

from openmdao.examples.nozzle_geometry_doe.simple_nozzle import PlugNozzleGeometry
from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import LatinHypercube

class NozzleGeometryDOE(Assembly):
    """DOE of the PlugNozzleGeometry Component."""

    def configure(self):
        """Creates a new Assembly containing a PlugNozzleGeometry and DOE"""

        self.replace("driver", DOEdriver())

        self.driver.add("DOEgenerator",LatinHypercube(num_samples=5))

        self.add("plug_noz", PlugNozzleGeometry())

        self.driver.add_parameter('plug_noz.cowl.thickness[7]', low=0.0, high=0.5)
        self.driver.add_parameter('plug_noz.cowl.thickness[8]', low=0.0, high=0.5)
        self.driver.add_parameter('plug_noz.cowl.R[7]', low=-0.1, high=0.2)
        self.driver.add_parameter('plug_noz.cowl.R[8]', low=-0.1, high=0.2)
        self.driver.add_parameter('plug_noz.plug.R[7]', low=-0.1, high=0.5)
        self.driver.add_parameter('plug_noz.plug.R[8]', low=-0.1, high=0.5)



if __name__ == "__main__": # pragma: no cover

    import time

    doe_problem = NozzleGeometryDOE()

    tt = time.time()
    doe_problem.run()

    print "\n"
    print "Elapsed time: ", time.time()-tt, "seconds"

