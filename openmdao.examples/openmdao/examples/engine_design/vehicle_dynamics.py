# VehicleDynamics.py
#
# This openMDAO component determines the vehicle acceleration based on the
# power output of the engine, modified by the transmission torque ratio.

from openmdao.main import Component, Float
from openmdao.main.variable import INPUT, OUTPUT
from math import pi

class VehicleDynamics(Component):
    ''' A vehicle dynamics component - calculates acceleration.'''
    
    def __init__(self, name, parent=None, doc=None, directory=''):
        ''' Creates a new VehicleDynamics object
        
            # Design parameters
            mass_vehicle        # Vehicle Mass (kg)
            Cf                  # Friction coef (proportional to V)
            Cd                  # Drag coef (proportional to V**2)
            area                # Frontal area (for drag calc) (sq m)
            
            # Simulation inputs
            mass_engine         # Engine Mass (kg)
            velocity            # Vehicle velocity (m/s)
            engine_torque       # Engine Torque (Nm)
            torque_ratio        # Torque ratio due to Transmission
            tire_circ           # Circumference of tire (m)
            
            # Outputs
            acceleration        # Calculated vehicle acceleration (m/s^2)
            
            '''
        
        super(VehicleDynamics, self).__init__(name, parent, doc, 
                                               directory)        
        
        # set up interface to the framework  
        # Pylint: disable-msg=E1101
        Float('mass_vehicle', self, INPUT, units='kg', default=1200.0,
              doc='Vehicle Mass')
        Float('Cf', self, INPUT, units=None, default=0.035,
              doc='Friction Coefficient (proportional to W)')
        Float('Cd', self, INPUT, units=None, default=0.3,
              doc='Drag Coefficient (proportional to V**2)')
        Float('area', self, INPUT, units='m**2', default=2.164,
              doc='Frontal area')

        Float('engine_torque', self, INPUT, units='N*m', default=200.0,
              doc='Torque at engine output')
        Float('mass_engine', self, INPUT, units='kg', default=200.0,
              doc='Engine weight estimation')
        Float('velocity', self, INPUT, units='m/s', default=0.0,
              doc='Current Velocity of Vehicle')
        Float('torque_ratio', self, INPUT, units=None, default=0.0, 
              doc='Ratio of output torque to engine torque')        
        Float('tire_circ', self, INPUT, units='m', default=1.905,
              doc='Circumference of tire')
        
        Float('acceleration', self, OUTPUT, units='m/(s*s)', default=0.0, 
              doc='Calculated vehicle acceleration ')        
        
    def execute(self):
        ''' Calculates the instantaneous acceleration for the vehicle.       
            '''        
        #print '%s.execute()' % self.get_pathname()
        torque = self.engine_torque*self.torque_ratio
        tire_radius = self.tire_circ/(2.0*pi)
        
        mass = self.mass_vehicle + self.mass_engine
        
        friction = self.Cf*mass*9.8
        drag = .5*(1.225)*self.Cd*self.area*self.velocity*self.velocity
        
        self.acceleration = (torque/tire_radius - friction - drag)/mass
        
# End VehicleDynamics.py
