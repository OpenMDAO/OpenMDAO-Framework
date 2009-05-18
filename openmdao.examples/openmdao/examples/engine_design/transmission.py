# transmission.py
#
# This openMDAO component contains a simple transmission model
# Transmission is a 5-speed manual.

from openmdao.main import Component, Float, Int
from openmdao.main.variable import INPUT, OUTPUT

class Transmission(Component):
    def __init__(self, name, parent=None, doc=None, directory=''):
        ''' Creates a new Transmission object
        
            # Design parameters
            Ratio1              # Gear Ratio in First Gear
            Ratio2              # Gear Ratio in Second Gear
            Ratio3              # Gear Ratio in Third Gear
            Ratio4              # Gear Ratio in Fourth Gear
            Ratio5              # Gear Ratio in Fifth Gear
            FinalDriveRatio     # Final Drive Ratio
            TireCircumference   # Circumference of tire (inches)
            
            # Simulation inputs
            CurrentGear         # Gear Position
            Velocity            # Vehicle velocity needed to determine engine
                                  RPM (m/s)
            
            # Outputs
            TorqueRatio         # Ratio of output torque to engine torque
            EngineRPM           # EngineRPM 
            '''
        
        super(Transmission, self).__init__(name, parent, doc, directory)        
        
        # set up interface to the framework  
        # Pylint: disable-msg=E1101
        Float('Ratio1', self, INPUT, units=None, default=3.54,
              doc='Gear Ratio in First Gear')
        Float('Ratio2', self, INPUT, units=None, default=2.13,
              doc='Gear Ratio in Second Gear')
        Float('Ratio3', self, INPUT, units=None, default=1.36,
              doc='Gear Ratio in Third Gear')
        Float('Ratio4', self, INPUT, units=None, default=1.03,
              doc='Gear Ratio in Fourth Gear')
        Float('Ratio5', self, INPUT, units=None, default=0.72,
              doc='Gear Ratio in Fifth Gear')
        Float('FinalDriveRatio', self, INPUT, units=None, default=2.80,
              doc='Final Drive Ratio')
        Float('TireCirc', self, INPUT, units='inch', default=75.0,
              doc='Circumference of tire (inches)')

        Int('CurrentGear', self, INPUT, default=0,
              doc='Current Gear')
        Float('Velocity', self, INPUT, units='mi/h', default=0.0,
              doc='Current Velocity of Vehicle')

        Float('RPM', self, OUTPUT, units='1/min', default=1000.0, 
              doc='Engine RPM')        
        Float('TorqueRatio', self, OUTPUT, units=None, default=0.0, 
              doc='Ratio of output torque to engine torque')        
        
    def execute(self):
        ''' The 5-speed manual transmission is simulated by determining the
            torque output and engine RPM via the gear ratios.
            '''
        #print '%s.execute()' % self.get_pathname()
        Ratios = [0.0, self.Ratio1, self.Ratio2, self.Ratio3, self.Ratio4,
                  self.Ratio5]
        
        Gear = self.CurrentGear
        
        self.RPM = (Ratios[Gear]*self.FinalDriveRatio*5280.0*12.0*self.Velocity
                    )/(60.0*self.TireCirc)
        self.TorqueRatio = Ratios[Gear]*self.FinalDriveRatio
            
        # At low speeds, hold engine speed at 1000 RPM and feather the clutch
        if self.RPM < 1000.0 and self.CurrentGear == 1 :
            self.RPM = 1000.0
        
# End Transmission.py
