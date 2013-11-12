"""
    bar3_optimization.py - Top level assembly for the example problem.
"""

# Optimize the bar3 design using the CONMIN optimizer.

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import CONMINdriver
from openmdao.main.datatypes.api import Float

#from openmdao.examples.bar3simulation.bar3 import Bar3Truss
from openmdao.examples.bar3simulation.bar3_wrap_f import Bar3Truss

class Bar3Optimization(Assembly):
    """ Optimization of a three bar truss. """

    # set up interface to the framework  
    # pylint: disable-msg=E1101
    
    # Constraint allowables
    bar1_stress_allowable = Float(20., iotype='in',
                        units='lb/(inch*inch)',
                        desc='Stress allowable in bar 1')
    bar2_stress_allowable = Float(20., iotype='in',
                        units='lb/(inch*inch)',
                        desc='Stress allowable in bar 2')
    bar3_stress_allowable = Float(20., iotype='in', 
                        units='lb/(inch*inch)',
                        desc='Stress allowable in bar 3')
    displacement_x_dir_allowable = Float(0.20, iotype='in', units='inch',
                       desc='Displacement limitation in x-direction')
    displacement_y_dir_allowable = Float(0.05, iotype='in', units='inch',
                        desc='Displacement limitation in y-direction')
    frequency_allowable = Float(14.1421, iotype='in', units='Hz',
                        desc='Frequency limitation in Hertz')
    
    def configure(self):

        # Create CONMIN Optimizer instance
        self.add('driver', CONMINdriver())
        
        # Create Bar3_Truss component instances
        self.add('bar3_truss', Bar3Truss())
        
        self.driver.workflow.add('bar3_truss')

        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.itmax = 30
        self.driver.fdch = .00001
        self.driver.fdchm = .00001
        self.driver.ct = -.001
        
        # CONMIN Objective 
        self.driver.add_objective('bar3_truss.weight')
        
        # CONMIN Design Variables 
        for param, low, high in zip(['bar3_truss.bar1_area', 
                                        'bar3_truss.bar2_area',
                                        'bar3_truss.bar3_area'],
                                    [0.001, 0.001, 0.001],
                                    [10000.0, 10000.0, 10000.0]):
            self.driver.add_parameter(param, low=low, high=high)

       # CONMIN Constraints

        constraints = [
            'abs(bar3_truss.bar1_stress/bar1_stress_allowable) <= 1.0',
            'abs(bar3_truss.bar2_stress/bar2_stress_allowable) <= 1.0',
            'abs(bar3_truss.bar3_stress/bar3_stress_allowable) <= 1.0',
            'abs(bar3_truss.displacement_x_dir/displacement_x_dir_allowable) <= 1.0',
            'abs(bar3_truss.displacement_y_dir/displacement_y_dir_allowable) <= 1.0',
            'frequency_allowable**2 <= bar3_truss.frequency**2']
        map(self.driver.add_constraint, constraints)
        

if __name__ == "__main__": # pragma: no cover         

    import time

    # pylint: disable-msg=E1101

    opt_bar3 = Bar3Optimization()

    def prz(title):
        """ Print before and after"""
        
        print '---------------------------------'
        print title
        print '---------------------------------'
        print 'Bar3: Weight = ', opt_bar3.bar3_truss.weight
        print 'DV1: Bar1_area = ', opt_bar3.bar3_truss.bar1_area
        print 'DV2: Bar2_area = ', opt_bar3.bar3_truss.bar2_area
        print 'Dv3: Bar3_area = ', opt_bar3.bar3_truss.bar3_area
        print '---------------------------------'
        print 'Con1: Bar1_stress = ', opt_bar3.bar3_truss.bar1_stress
        print 'Con2: Bar2_stress = ', opt_bar3.bar3_truss.bar2_stress
        print 'Con3: Bar3_stress = ', opt_bar3.bar3_truss.bar3_stress
        print 'Con4: Displ_u = ', opt_bar3.bar3_truss.displacement_x_dir
        print 'Con5: Displ_v = ', opt_bar3.bar3_truss.displacement_y_dir
        print 'Con6: Frequency = ', opt_bar3.bar3_truss.frequency
        print '\n'
    

    opt_bar3.bar3_truss.run()
    prz('Old Design')

    time1 = time.time()
    opt_bar3.run()
    prz('New Design')
    print "CONMIN Iterations: ", opt_bar3.driver.iter_count
    print ""
    print "Elapsed time: ", time.time()-time1
    
# end bar3_optimization.py
