# Vehicle_Dynamics.py
#
# This openMDAO component determines the vehicle acceleration based on the
# power output of the engine, modified by the transmission torque ratio.

from openmdao.main import Component, Float
from openmdao.main.variable import INPUT, OUTPUT
from math import pi

class Vehicle_Dynamics(Component):
    def __init__(self, name, parent=None, doc=None, directory=''):
        ''' Creates a new Vehicle_Dynamics object
        
            # Design parameters
            Mass_Vehicle        # Vehicle Mass (kg)
            Cf                  # Friction coef (proportional to V)
            Cd                  # Drag coef (proportional to V**2)
            Area                # Frontal area (for drag calc) (sq m)
            
            # Simulation inputs
            Mass_Engine         # Engine Mass (kg)
            Velocity            # Vehicle velocity (m/s)
            Engine_Torque       # Engine Torque (Nm)
            Torque_Ratio        # Torque ratio due to Transmission
            TireCircumference   # Circumference of tire (m)
            
            # Outputs
            Acceleration        # Calculated vehicle acceleration (m/s^2)
            
            '''
        
        super(Vehicle_Dynamics, self).__init__(name, parent, doc, directory)        
        
        # set up interface to the framework  
        # Pylint: disable-msg=E1101
        Float('Mass_Vehicle', self, INPUT, units='kg', default=1200.0,
              doc='Vehicle Mass')
        Float('Cf', self, INPUT, units=None, default=0.035,
              doc='Friction Coefficient (proportional to W)')
        Float('Cd', self, INPUT, units=None, default=0.3,
              doc='Drag Coefficient (proportional to V**2)')
        Float('Area', self, INPUT, units='m**2', default=2.164,
              doc='Frontal area')

        Float('Engine_Torque', self, INPUT, units='N*m', default=200.0,
              doc='Torque at engine output')
        Float('Mass_Engine', self, INPUT, units='kg', default=200.0,
              doc='Engine weight estimation')
        Float('Velocity', self, INPUT, units='m/s', default=0.0,
              doc='Current Velocity of Vehicle')
        Float('Torque_Ratio', self, INPUT, units=None, default=0.0, 
              doc='Ratio of output torque to engine torque')        
        Float('TireCirc', self, INPUT, units='m', default=2.0,
              doc='Circumference of tire')
        
        Float('Acceleration', self, OUTPUT, units='m/(s*s)', default=0.0, 
              doc='Calculated vehicle acceleration ')        
        
    def execute(self):
        ''' Calculates the instantaneous acceleration for the vehicle.       
            '''        
        print '%s.execute()' % self.get_pathname()
        Torque = self.Engine_Torque*self.Torque_Ratio
        TireRadius = self.TireCirc/(2.0*pi)
        
        Mass = self.Mass_Vehicle + self.Mass_Engine
        
        Friction = self.Cf*Mass*9.8
        Drag = .5*(1.225)*self.Cd*self.Area*self.Velocity*self.Velocity
        
        self.Acceleration = (Torque/TireRadius - Friction - Drag)/Mass
        
# End Vehicle_Dynamics.py
