# transmission.py
#
# This openMDAO component contains a simple transmission model
# Transmission is a 5-speed manual.

from enthought.traits.api import Float, Int
from openmdao.main.api import Component

class Transmission(Component):
    ''' A simple transmission model.'''
    
    def __init__(self, name, parent=None, doc=None, directory=''):
        ''' Creates a new Transmission object
        
            # Design parameters
            ratio1              # Gear ratio in First Gear
            ratio2              # Gear ratio in Second Gear
            ratio3              # Gear ratio in Third Gear
            ratio4              # Gear ratio in Fourth Gear
            ratio5              # Gear ratio in Fifth Gear
            final_drive_ratio   # Final Drive Ratio
            tire_circumference  # Circumference of tire (inches)
            
            # Simulation inputs
            current_gear        # Gear Position
            velocity            # Vehicle velocity needed to determine engine
                                  RPM (m/s)
            
            # Outputs
            torque_ratio        # Ratio of output torque to engine torque
            RPM                 # RPM of the engine
            '''
        
        super(Transmission, self).__init__(name, parent, doc, directory)        
        
        # set up interface to the framework  
        # Pylint: disable-msg=E1101
        Float('ratio1', self, INPUT, units=None, default=3.54,
              desc='Gear ratio in First Gear')
        Float('ratio2', self, iostatus='in', units=None, default=2.13,
              desc='Gear ratio in Second Gear')
        Float('ratio3', self, iostatus='in', units=None, default=1.36,
              desc='Gear ratio in Third Gear')
        Float('ratio4', self, iostatus='in', units=None, default=1.03,
              desc='Gear ratio in Fourth Gear')
        Float('ratio5', self, iostatus='in', units=None, default=0.72,
              desc='Gear ratio in Fifth Gear')
        Float('final_drive_ratio', self, iostatus='in', units=None, default=2.80,
              desc='Final Drive Ratio')
        Float('tire_circ', self, iostatus='in', units='inch', default=75.0,
              desc='Circumference of tire (inches)')

        Int('current_gear', self, iostatus='in', default=0,
              desc='Current Gear')
        Float('velocity', self, iostatus='in', units='mi/h', default=0.0,
              desc='Current Velocity of Vehicle')

        Float('RPM', self, iostatus='out', units='1/min', default=1000.0, 
              desc='Engine RPM')        
        Float('torque_ratio', self, iostatus='out', units=None, default=0.0, 
              desc='Ratio of output torque to engine torque')        
        
    def execute(self):
        ''' The 5-speed manual transmission is simulated by determining the
            torque output and engine RPM via the gear ratios.
            '''
        #print '%s.execute()' % self.get_pathname()
        ratios = [0.0, self.ratio1, self.ratio2, self.ratio3, self.ratio4,
                  self.ratio5]
        
        gear = self.current_gear
        
        self.RPM = (ratios[gear]*self.final_drive_ratio*5280.0*12.0 \
                    *self.velocity)/(60.0*self.tire_circ)
        self.torque_ratio = ratios[gear]*self.final_drive_ratio
            
        # At low speeds, hold engine speed at 1000 RPM and feather the clutch
        if self.RPM < 1000.0 and self.current_gear == 1 :
            self.RPM = 1000.0
        
# End Transmission.py
