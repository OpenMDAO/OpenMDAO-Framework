"""
    bar3_optimization.py - Top level assembly for the example problem.
"""

# Optimize the bar3 design using the driving_sim component.

from openmdao.main.api import Assembly
from openmdao.lib.traits.unitsfloat import UnitsFloat
from openmdao.lib.drivers.conmindriver import CONMINdriver

#from openmdao.examples.bar3_simulation.bar3 import Bar3Truss
from openmdao.examples.bar3_simulation.bar3_wrap_f import Bar3Truss

class Bar3Optimization(Assembly):
    """ Top level assembly for optimizing a three bar truss. """
    # Constraint allowables
    bar1_stress_allowable = UnitsFloat(20., iostatus='in', units='lb/(inch*inch)',
                        desc='Stress allowable in bar 1')
    bar2_stress_allowable = UnitsFloat(20., iostatus='in', units='lb/(inch*inch)',
                        desc='Stress allowable in bar 2')
    bar3_stress_allowable = UnitsFloat(20., iostatus='in', units='lb/(inch*inch)',
                        desc='Stress allowable in bar 3')
    displacement_x_dir_allowable = UnitsFloat(0.20, iostatus='in', units='inch',
                       desc='Displacement limitation in x-direction')
    displacement_y_dir_allowable = UnitsFloat(0.05, iostatus='in', units='inch',
                        desc='Displacement limitation in y-direction')
    frequency_Hz_allowable = UnitsFloat(14.1421, iostatus='in', units='Hz',
                        desc='Frequency limitation in Hertz')
    
    def __init__(self, directory=''):
        """ Creates a new Assembly containing a Bar3_Truss and an optimizer"""
        
        super(Bar3Optimization, self).__init__(directory)

        # Create Bar3_Truss component instances
        self.add_container('bar3_truss', Bar3Truss())

        # Create CONMIN Optimizer instance
        self.add_container('driver', CONMINdriver())
        
        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.itmax = 30
        self.driver.fdch = .00001
        self.driver.fdchm = .00001
        self.driver.ct = -.001

        
        # CONMIN Objective 
        self.driver.objective = 'bar3_truss.weight'
        
        # CONMIN Design Variables 
        self.driver.design_vars = ['bar3_truss.bar1_area', 
                                   'bar3_truss.bar2_area',
                                   'bar3_truss.bar3_area']
        
        self.driver.lower_bounds = [0.001, 0.001, 0.001]
        self.driver.upper_bounds = [10000.0, 10000.0, 10000.0]
        
       

       # CONMIN Constraints

        bar1_stress_allowable = self.bar1_stress_allowable
        bar2_stress_allowable = self.bar2_stress_allowable
        bar3_stress_allowable = self.bar3_stress_allowable
        displacement_x_dir_allowable = self.displacement_x_dir_allowable
        displacement_y_dir_allowable = self.displacement_y_dir_allowable
        frequency_Hz_allowable = self.frequency_Hz_allowable

        self.driver.constraints = ['(bar3_truss.bar1_stress/bar1_stress_allowable)-1.0',
                                   '(bar3_truss.bar2_stress/bar2_stress_allowable)-1.0',
                                   '(bar3_truss.bar3_stress/bar3_stress_allowable)-1.0',
                                   '(bar3_truss.displacement_x_dir/displacement_x_dir_allowable)-1.0',
                                   '(bar3_truss.displacement_y_dir/displacement_y_dir_allowable)-1.0',
                                   '(frequency_Hz_allowable*frequency_Hz_allowable)/(bar3_truss.frequency_Hz*bar3_truss.frequency_Hz) - 1.0']
 

if __name__ == "__main__": # pragma: no cover         

    import time

    opt_bar3 = Bar3Optimization("Top")
 
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
        print 'Con6: Frequency = ', opt_bar3.bar3_truss.frequency_Hz
        print '\n'
    

    opt_bar3.bar3_truss.run()
    prz('Old Design')

    tt = time.time()
    opt_bar3.run()
    prz('New Design')
    print "CONMIN Iterations: ", opt_bar3.driver.get("iter_count")
    print ""
    print "Elapsed time: ", time.time()-tt
    
# end bar3_optimization.py
