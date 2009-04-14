# Vehicle.py
#
# Model that contains an engine, a transmission, and a vehicle_dynamics
# component. Together, these output the acceleration for a set of input
# the velocity and commanded throttle/gear positions given a set of design.
# parameters.

from zope.interface import implements, Interface

from openmdao.main import Model
from openmdao.main import Float, Int
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.interfaces import IComponent

from openmdao.examples.engine_design.engine import Engine
from openmdao.examples.engine_design.transmission import Transmission
from openmdao.examples.engine_design.vehicle_dynamics import Vehicle_Dynamics

class IVehicle(Interface):
    """Vehicle Model interface"""
    
class Vehicle(Model):
    """ Vehicle model. """
    
    implements(IVehicle)
    
    def __init__(self, name, parent=None, directory=''):
        ''' Creates a new Vehicle Model object

            # Design parameters promoted from Engine
            stroke = 78.8              # Stroke (mm)
            bore = 82.0                # Bore (mm)
            conrod = 115.0             # Connecting Rod (mm)
            compRatio = 9.3            # Compression Ratio
            sparkAngle = -37.0         # Spark Angle ref TDC (degree)
            nCyl = 6                   # Number of Cylinders
            IVO = 11.0                 # Intake Valve Open before TDC (degree BTDC)
            IVC = 53.0                 # Intake Valve Close after BDC (degree ABDC)
            Liv = 8.0                  # Maximum Valve Lift (mm)
            Div = 41.2                 # Inlet Valve Dia (mm)
            
            # Design parameters from Transmission
            Ratio1                     # Gear Ratio in First Gear
            Ratio2                     # Gear Ratio in Second Gear
            Ratio3                     # Gear Ratio in Third Gear
            Ratio4                     # Gear Ratio in Fourth Gear
            Ratio5                     # Gear Ratio in Fifth Gear
            FinalDriveRatio            # Final Drive Ratio
            TireCircumference          # Circumference of tire (inches)
            
            # Design parameters from Vehicle Dynamics
            Mass_Vehicle               # Vehicle Mass (kg)
            Cf                         # Friction coef (proportional to V)
            Cd                         # Drag coef (proportional to V**2)
            Area                       # Frontal area (for drag calc) (sq m)
            
            # Simulation Inputs
            CurrentGear                # Gear Position
            throttle                   # Throttle Position
            Velocity                   # Vehicle velocity needed to determine
                                         engine RPM (m/s)
            
            # Outputs
            Power                      # Power at engine output (KW)
            Torque                     # Torque at engine output (N*m)
            FuelBurn                   # Fuel burn rate (liters/sec)
            Acceleration               # Calculated vehicle acceleration (m/s^2)
            '''
        
        super(Vehicle, self).__init__(name, parent, directory)

        # Create component instances
        
        self.add_socket('Transmission', IComponent, required=True)
        comp1 = Transmission('Transmission', parent=self)
        self.add_child(comp1)
        self.workflow.add_node(comp1)
        
        self.add_socket('Engine', IComponent, required=True)
        comp2 = Engine('Engine', parent=self)
        self.add_child(comp2)
        self.workflow.add_node(comp2)

        self.add_socket('VDyn', IComponent, required=True)
        comp3 = Vehicle_Dynamics('VDyn', parent=self)
        self.add_child(comp3)
        self.workflow.add_node(comp3)

        # Create input and output ports at the model level
        # pylint: disable-msg=E1101
        # "Instance of <class> has no <attr> member"        
        
        # Promoted From Engine
        self.create_passthru('Engine.stroke')
        self.create_passthru('Engine.bore')
        self.create_passthru('Engine.conrod')
        self.create_passthru('Engine.compRatio')
        self.create_passthru('Engine.sparkAngle')
        self.create_passthru('Engine.nCyl')
        self.create_passthru('Engine.IVO')
        self.create_passthru('Engine.IVC')
        self.create_passthru('Engine.Liv')
        self.create_passthru('Engine.Div')

        # Promoted From Transmission
        self.create_passthru('Transmission.Ratio1')
        self.create_passthru('Transmission.Ratio2')
        self.create_passthru('Transmission.Ratio3')
        self.create_passthru('Transmission.Ratio4')
        self.create_passthru('Transmission.Ratio5')
        self.create_passthru('Transmission.FinalDriveRatio')
        self.create_passthru('Transmission.TireCirc')

        # Promoted From Vehicle_Dynamics
        self.create_passthru('VDyn.Mass_Vehicle')
        self.create_passthru('VDyn.Cf')
        self.create_passthru('VDyn.Cd')
        self.create_passthru('VDyn.Area')
        # NOTE: Tire Circumference also needed by Transmission.

        # Vehicle Simulation Inputs
        Int('CurrentGear', self, INPUT, default=0,
              doc='Current Gear')
        Float('Velocity', self, INPUT, units='mi/h', default=0.0,
              doc='Current Velocity of Vehicle')
        Float('Throttle', self, INPUT, units=None, default=1.0, min_limit=0.01,
              max_limit=1.0, doc='Throttle position (from low idle to wide open)')

        # Vehicle Simulation Outputs
        Float('Power', self, OUTPUT, units='kW', default=0.0,
              doc='Power at engine output')
        Float('Torque', self, OUTPUT, units='N*m', default=0.0,
              doc='Torque at engine output')
        Float('FuelBurn', self, OUTPUT, units='l/s', default=0.0,
              doc='Torque at engine output')
        Float('Acceleration', self, OUTPUT, units='m/(s*s)', default=0.0, 
              doc='Calculated vehicle acceleration ')        
        
        # Hook it all up
        
        self.connect('Transmission.RPM','Engine.RPM')
        self.connect('Transmission.TorqueRatio','VDyn.Torque_Ratio')
        self.connect('Engine.Torque','VDyn.Engine_Torque')
        self.connect('Engine.EngineWeight','VDyn.Mass_Engine')

        # passthru connections
        self.connect('CurrentGear','Transmission.CurrentGear')
        self.connect('Velocity','Transmission.Velocity')
        self.connect('Throttle','Engine.Throttle')
        self.connect('Velocity','VDyn.Velocity')
        self.connect('Engine.Power','Power')
        self.connect('Engine.Torque','Torque')
        self.connect('Engine.FuelBurn','FuelBurn')
        self.connect('VDyn.Acceleration','Acceleration')

        
if __name__ == "__main__":        
    z = Vehicle("Testing")        
    z.CurrentGear = 3
    z.Velocity = 60.0*(26.8224/60.0)
    z.Throttle = .2
    z.run()
    
    def prz(zz):
        print "Accel = ", zz.Acceleration
        print "Fuelburn = ", zz.FuelBurn
        print "(Power, Torque) ", zz.Power, zz.Torque
        print "RPM = ", zz.Engine.RPM
        
    prz(z)

# End vehicle.py 
