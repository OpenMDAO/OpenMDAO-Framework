"""
    transmission.py - Transmission component for the vehicle example problem.
"""

# This openMDAO component contains a simple transmission model
# Transmission is a 5-speed manual.

from enthought.traits.api import Float, Int, Range
from openmdao.main.api import Component
from openmdao.lib.traits.unitsfloat import UnitsFloat

class Transmission(Component):
    """ A simple transmission model."""
    
    # set up interface to the framework  
    # Pylint: disable-msg=E1101
    ratio1 = Float(3.54, iotype='in', 
                   desc='Gear ratio in First Gear')
    ratio2 = Float(2.13, iotype='in', 
                   desc='Gear ratio in Second Gear')
    ratio3 = Float(1.36, iotype='in', 
                   desc='Gear ratio in Third Gear')
    ratio4 = Float(1.03, iotype='in', 
                   desc='Gear ratio in Fourth Gear')
    ratio5 = Float(0.72, iotype='in', 
                   desc='Gear ratio in Fifth Gear')
    final_drive_ratio = Float(2.8, iotype='in', 
                              desc='Final Drive Ratio')
    tire_circ = UnitsFloat(75.0, iotype='in', units='inch', 
                           desc='Circumference of tire (inches)')

    current_gear = Range(value=0, iotype='in', low=0, high=5, \
                          desc='Current Gear')
    velocity = UnitsFloat(0., iotype='in', units='mi/h',
                     desc='Current Velocity of Vehicle')

    RPM = UnitsFloat(1000., iotype='out', units='1/min',
                     desc='Engine RPM')        
    torque_ratio = Float(0., iotype='out',
                         desc='Ratio of output torque to engine torque')        

    #def __init__(self, doc=None, directory=''):
        #""" Creates a new Transmission object
        
            ## Design parameters
            #ratio1              # Gear ratio in First Gear
            #ratio2              # Gear ratio in Second Gear
            #ratio3              # Gear ratio in Third Gear
            #ratio4              # Gear ratio in Fourth Gear
            #ratio5              # Gear ratio in Fifth Gear
            #final_drive_ratio   # Final Drive Ratio
            #tire_circumference  # Circumference of tire (inches)
            
            ## Simulation inputs
            #current_gear        # Gear Position
            #velocity            # Vehicle velocity needed to determine engine
                                  #RPM (m/s)
            
            ## Outputs
            #torque_ratio        # Ratio of output torque to engine torque
            #RPM                 # RPM of the engine
            #"""
        
        #super(Transmission, self).__init__(doc, directory)        
        
        
    def execute(self):
        """ The 5-speed manual transmission is simulated by determining the
            torque output and engine RPM via the gear ratios.
            """
        #print '%s.execute()' % self.get_pathname()
        ratios = [0.0, self.ratio1, self.ratio2, self.ratio3, self.ratio4,
                  self.ratio5]
        
        gear = self.current_gear
        differential = self.final_drive_ratio
        
        self.RPM = (ratios[gear]*differential*5280.0*12.0 \
                    *self.velocity)/(60.0*self.tire_circ)
        self.torque_ratio = ratios[gear]*differential
            
        # At low speeds, hold engine speed at 1000 RPM and
        # partially engage clutch
        if self.RPM < 1000.0 and self.current_gear == 1 :
            self.RPM = 1000.0
        
# End Transmission.py
