"""
    vehicle.py - Vehicle component for the vehicle example problem.
"""

# Assembly that contains an engine, a transmission, and a chasis
# component. Together, these output the acceleration for a set of input
# the velocity and commanded throttle/gear positions given a set of design.
# parameters.

from enthought.traits.api import implements, Interface

from openmdao.main.api import Assembly, UnitsFloat, set_as_top

from openmdao.examples.engine_design.transmission import Transmission
from openmdao.examples.engine_design.chasis import Chasis
try:
    from openmdao.examples.engine_design.engine_wrap_c import Engine
except:
    from openmdao.examples.engine_design.engine import Engine

    
class IVehicle(Interface):
    """Vehicle Model interface"""
    
class Vehicle(Assembly):
    """ Vehicle assembly. """
    
    implements(IVehicle)
    
    tire_circumference = UnitsFloat(75.0, iostatus='in', units='inch', 
                                    desc='Circumference of tire (inches)')
    
    velocity = UnitsFloat(75.0, iostatus='in', units='mi/h', 
                desc='Vehicle velocity needed to determine engine RPM (mi/h)')
    
    def __init__(self, directory=''):
        """ Creates a new Vehicle Assembly object

            # Design parameters promoted from Engine
            stroke = 78.8              # Stroke (mm)
            bore = 82.0                # Bore (mm)
            conrod = 115.0             # Connecting Rod (mm)
            comp_ratio = 9.3           # Compression Ratio
            spark_angle = -37.0        # Spark Angle ref TDC (degree)
            n_cyl = 6                  # Number of Cylinders
            IVO = 11.0                 # Intake Valve Open before TDC (deg BTDC)
            IVC = 53.0                 # Intake Valve Close after BDC (deg ABDC)
            L_v = 8.0                  # Maximum Valve Lift (mm)
            D_v = 41.2                 # Inlet Valve Dia (mm)
            
            # Design parameters from Transmission
            ratio1                     # Gear ratio in First Gear
            ratio2                     # Gear ratio in Second Gear
            ratio3                     # Gear ratio in Third Gear
            ratio4                     # Gear ratio in Fourth Gear
            ratio5                     # Gear ratio in Fifth Gear
            final_drive_ratio          # Final Drive Ratio
            tire_circumference         # Circumference of tire (inches)
            
            # Design parameters from Vehicle Dynamics
            mass_vehicle               # Vehicle Mass (kg)
            Cf                         # Friction coef (proportional to V)
            Cd                         # Drag coef (proportional to V**2)
            area                       # Frontal area (for drag calc) (sq m)
            
            # Simulation Inputs
            current_gear               # Gear Position
            throttle                   # Throttle Position
            velocity                   # Vehicle velocity needed to determine
                                         engine RPM (mi/h)
            
            # Outputs
            power                      # Power at engine output (KW)
            torque                     # Torque at engine output (N*m)
            fuel_burn                  # Fuel burn rate (liters/sec)
            acceleration               # Calculated vehicle acceleration (m/s^2)
            """
        
        super(Vehicle, self).__init__(directory)

        # Create component instances
        
        self.add_container('transmission', Transmission())
        self.add_container('engine', Engine())
        self.add_container('chasis', Chasis())

        # Create input and output ports at the assembly level
        # pylint: disable-msg=E1101
        # "Instance of <class> has no <attr> member"        
        
        # Promoted From Engine
        self.create_passthru('engine.stroke')
        self.create_passthru('engine.bore')
        self.create_passthru('engine.conrod')
        self.create_passthru('engine.comp_ratio')
        self.create_passthru('engine.spark_angle')
        self.create_passthru('engine.n_cyl')
        self.create_passthru('engine.IVO')
        self.create_passthru('engine.IVC')
        self.create_passthru('engine.L_v')
        self.create_passthru('engine.D_v')
        self.create_passthru('engine.throttle')
        self.create_passthru('engine.power')
        self.create_passthru('engine.torque')
        self.create_passthru('engine.fuel_burn')

        # Promoted From Transmission
        self.create_passthru('transmission.ratio1')
        self.create_passthru('transmission.ratio2')
        self.create_passthru('transmission.ratio3')
        self.create_passthru('transmission.ratio4')
        self.create_passthru('transmission.ratio5')
        self.create_passthru('transmission.final_drive_ratio')
        self.create_passthru('transmission.current_gear')

        # Promoted From Chasis
        self.create_passthru('chasis.mass_vehicle')
        self.create_passthru('chasis.Cf')
        self.create_passthru('chasis.Cd')
        self.create_passthru('chasis.area')
        
        self.connect('velocity', 'chasis.velocity')
        self.connect('velocity', 'transmission.velocity')
        self.connect('tire_circumference', 'chasis.tire_circ')
        self.connect('tire_circumference', 'transmission.tire_circ')
        self.create_passthru('chasis.acceleration')

        # Hook it all up
        
        self.connect('transmission.RPM','engine.RPM')
        self.connect('transmission.torque_ratio','chasis.torque_ratio')
        self.connect('engine.torque','chasis.engine_torque')
        self.connect('engine.engine_weight','chasis.mass_engine')


        
if __name__ == "__main__": # pragma: no cover    
    top = set_as_top(Assembly())
    z = top.add_container('Testing', Vehicle())      
    z.current_gear = 1
    z.velocity = 20.0*(26.8224/60.0)
    #z.throttle = .2
    #for throttle in xrange(1,101,1):
    #    z.throttle = throttle/100.0
    z.throttle = 1.0
    z.run()
    print z.acceleration
    
    def prz(zz):
        """ Printing the results"""
        print "Accel = ", zz.acceleration
        print "Fuelburn = ", zz.fuel_burn
        print "(power, torque) ", zz.power, zz.torque
        print "RPM = ", zz.engine.RPM
        
#    prz(z)

# End vehicle.py 
