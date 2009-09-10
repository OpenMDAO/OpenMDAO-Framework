"""
    chasis.py - Chasis component for the vehicle example problem.
"""

# This openMDAO component determines the vehicle acceleration based on the
# power output of the engine, modified by the transmission torque ratio.

from math import pi

from enthought.traits.api import Float

from openmdao.main.api import Component
from openmdao.lib.traits.unitsfloat import UnitsFloat


class Chasis(Component):
    """ A vehicle dynamics component - calculates acceleration."""
    
    # set up interface to the framework  
    # Pylint: disable-msg=E1101
    mass_vehicle = UnitsFloat(1200.0, iostatus='in', units='kg', 
                              desc='Vehicle Mass')
    Cf = Float(0.035, iostatus='in', #units=None,
                    desc='Friction Coefficient (proportional to W)')
    Cd = Float(0.3, iostatus='in', #units=None, 
               desc='Drag Coefficient (proportional to V**2)')
    area = UnitsFloat(2.164, iostatus='in', units='m**2', 
                      desc='Frontal area')
    engine_torque = UnitsFloat(200.0, iostatus='in', units='N*m', 
                               desc='Torque at engine output')
    mass_engine = UnitsFloat(200.0, iostatus='in', units='kg',
                             desc='Engine weight estimation')
    velocity = UnitsFloat(0., iostatus='in', units='m/s', 
                          desc='Current Velocity of Vehicle')
    torque_ratio = Float(0., iostatus='in', #units=None, 
                         desc='Ratio of output torque to engine torque')        
    tire_circ = UnitsFloat(1.905, iostatus='in', units='m', 
                           desc='Circumference of tire')
    acceleration = UnitsFloat(0., iostatus='out', units='m/(s*s)', 
                              desc='Calculated vehicle acceleration ')    
        
            ## Design parameters
            #mass_vehicle        # Vehicle Mass (kg)
            #Cf                  # Friction coef (proportional to V)
            #Cd                  # Drag coef (proportional to V**2)
            #area                # Frontal area (for drag calc) (sq m)
            
            ## Simulation inputs
            #mass_engine         # Engine Mass (kg)
            #velocity            # Vehicle velocity (m/s)
            #engine_torque       # Engine Torque (Nm)
            #torque_ratio        # Torque ratio due to Transmission
            #tire_circ           # Circumference of tire (m)
            
            ## Outputs
            #acceleration        # Calculated vehicle acceleration (m/s^2)
        
        
        
    def execute(self):
        """ Calculates the instantaneous acceleration for the vehicle.       
            """        
        torque = self.engine_torque*self.torque_ratio
        tire_radius = self.tire_circ/(2.0*pi)
        
        mass = self.mass_vehicle + self.mass_engine
        V = self.velocity
        
        friction = self.Cf*mass*9.8
        drag = .5*(1.225)*self.Cd*self.area*V*V
        
        self.acceleration = (torque/tire_radius - friction - drag)/mass
        
# End Chasis.py
