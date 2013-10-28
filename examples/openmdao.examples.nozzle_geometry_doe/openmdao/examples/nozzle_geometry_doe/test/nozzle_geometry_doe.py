"""
    nozzle_geometry_doe.py - Top level assembly for nozzle geometry doe problem
"""

from openmdao.main.api import Assembly
from openmdao.main.factorymanager import create

class NozzleGeometryDOE(Assembly):
    """DOE of the PlugNozzleGeometry Component."""
    
    def configure(self):
        """ Creates a new Assembly containing a PlugNozzleGeometry and DOE"""
        
        self.replace("driver", create("openmdao.lib.drivers.doedriver.DOEdriver"))
        self.driver.add("DOEgenerator", create("openmdao.lib.doegenerators.optlh.LatinHypercube", num_samples=5))

        self.add("gc", create("openmdao.lib.components.geomcomp.GeomComponent"))
        self.gc.add("parametric_geometry", create("openmdao.examples.nozzle_geometry_doe.simple_nozzle.PlugNozzleGeometry"))
        self.driver.add_parameter('gc.cowl.thickness[7]',low=0.0,high=0.5);
        self.driver.add_parameter('gc.cowl.thickness[8]',low=0.0,high=0.5);
        self.driver.add_parameter('gc.cowl.R[7]',low=-0.1,high=0.2);
        self.driver.add_parameter('gc.cowl.R[8]',low=-0.1,high=0.2);
        self.driver.add_parameter('gc.plug.R[7]',low=-0.1,high=0.5);
        self.driver.add_parameter('gc.plug.R[8]',low=-0.1,high=0.5);

        self.add("sc", create("openmdao.lib.components.sleep_comp.SleepComponent"))
        self.driver.workflow.add("sc")
        self.set(u'sc.sleep_time', 1.0)

        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    doe_problem = NozzleGeometryDOE()
    
    tt = time.time()
    doe_problem.run()

    print "\n"
    print "Elapsed time: ", time.time()-tt, "seconds"
    
