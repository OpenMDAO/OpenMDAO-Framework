"""
    chassis.py - Chassis component for the vehicle example problem.
"""

# This openMDAO component determines the vehicle acceleration based on the
# power output of the engine, modified by the transmission torque ratio.

from math import pi

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.lib.api import Float


class Chassis(Component):
    """ A vehicle dynamics component - calculates acceleration."""

    # Design parameters:
    #mass_vehicle        # Vehicle Mass (kg)
    #Cf                  # Friction coef (proportional to V)
    #Cd                  # Drag coef (proportional to V**2)
    #area                # Frontal area (for drag calc) (sq m)
    
    # Simulation inputs:
    #mass_engine         # Engine Mass (kg)
    #velocity            # Vehicle velocity (m/s)
    #engine_torque       # Engine Torque (Nm)
    #torque_ratio        # Torque ratio due to Transmission
    #tire_circ           # Circumference of tire (m)
    
    # Outputs:
    #acceleration        # Calculated vehicle acceleration (m/s^2)
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    mass_vehicle = Float(1200.0, iotype='in', units='kg', 
                              desc='Vehicle Mass')
    Cf = Float(0.035, iotype='in', 
                    desc='Friction Coefficient (proportional to W)')
    Cd = Float(0.3, iotype='in', 
               desc='Drag Coefficient (proportional to V**2)')
    area = Float(2.164, iotype='in', units='m**2', 
                      desc='Frontal area')
    engine_torque = Float(200.0, iotype='in', units='N*m', 
                               desc='Torque at engine output')
    mass_engine = Float(200.0, iotype='in', units='kg',
                             desc='Engine weight estimation')
    velocity = Float(0., iotype='in', units='m/s', 
                          desc='Current Velocity of Vehicle')
    torque_ratio = Float(0., iotype='in', 
                         desc='Ratio of output torque to engine torque')        
    tire_circ = Float(1.905, iotype='in', units='m', 
                           desc='Circumference of tire')
    acceleration = Float(0., iotype='out', units='m/(s*s)', 
                              desc='Calculated vehicle acceleration ')    
        
        
    def execute(self):
        """ Calculates the instantaneous acceleration for the vehicle. """
        
        torque = self.engine_torque*self.torque_ratio
        tire_radius = self.tire_circ/(2.0*pi)
        
        mass = self.mass_vehicle + self.mass_engine
        V = self.velocity
        
        friction = self.Cf*mass*9.8
        drag = .5*(1.225)*self.Cd*self.area*V*V
        
        self.acceleration = (torque/tire_radius - friction - drag)/mass
        
# End Chassis.py
