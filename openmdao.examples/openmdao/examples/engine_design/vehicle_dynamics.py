# VehicleDynamics.py
#
# This openMDAO component determines the vehicle acceleration based on the
# power output of the engine, modified by the transmission torque ratio.

from openmdao.main.api import Component, Float
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
        Float('mass_vehicle', self, iostatus='in', units='kg', default=1200.0,
              desc='Vehicle Mass')
        Float('Cf', self, iostatus='in', units=None, default=0.035,
              desc='Friction Coefficient (proportional to W)')
        Float('Cd', self, iostatus='in', units=None, default=0.3,
              desc='Drag Coefficient (proportional to V**2)')
        Float('area', self, iostatus='in', units='m**2', default=2.164,
              desc='Frontal area')

        Float('engine_torque', self, iostatus='in', units='N*m', default=200.0,
              desc='Torque at engine output')
        Float('mass_engine', self, iostatus='in', units='kg', default=200.0,
              desc='Engine weight estimation')
        Float('velocity', self, iostatus='in', units='m/s', default=0.0,
              desc='Current Velocity of Vehicle')
        Float('torque_ratio', self, iostatus='in', units=None, default=0.0, 
              desc='Ratio of output torque to engine torque')        
        Float('tire_circ', self, iostatus='in', units='m', default=1.905,
              desc='Circumference of tire')
        
        Float('acceleration', self, iostatus='out', units='m/(s*s)', default=0.0, 
              desc='Calculated vehicle acceleration ')        
        
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
