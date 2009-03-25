# Vehicle.py
#
# Assembly that contains an engine, a transmission, and a vehicle_dynamics
# component. Together, these output the acceleration for a set of input
# the velocity and commanded throttle/gear positions given a set of design.
# parameters.

from openmdao.main import Assembly
from openmdao.main import Float, Int
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.component import RUN_OK

from engine import Engine
from transmission import Transmission
from vehicle_dynamics import Vehicle_Dynamics

class Vehicle(Assembly):
    """ Vehicle assembly. """
    
    def __init__(self, name, parent=None, directory=''):
        ''' Creates a new Vehicle Assembly object

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
            Acceleration               # Calculated vehicle acceleration (m/s)
            '''
        
        super(Vehicle, self).__init__(name, parent, directory)

        # Create component instances
        
        comp1 = Transmission('Transmission', parent=self)
        self.add_child(comp1)
        self.workflow.add_node(comp1)
        
        comp2 = Engine('Engine', parent=self)
        self.add_child(comp2)
        self.workflow.add_node(comp2)

        comp3 = Vehicle_Dynamics('VDyn', parent=self)
        self.add_child(comp3)
        self.workflow.add_node(comp3)

        # Create input and output ports at the assembly level
        # pylint: disable-msg=E1101
        # "Instance of <class> has no <attr> member"        
        
        # Promoted From Engine
        Float('stroke', self, INPUT, units='mm', default=78.8,
              doc='Cylinder Stroke')
        Float('bore', self, INPUT, units='mm', default=82.0, 
              doc='Cylinder Bore')
        Float('conrod', self, INPUT, units='mm', default=115.0, 
              doc='Connecting Rod Length')
        Float('compRatio', self, INPUT, units=None, default=9.3, 
              doc='Compression Ratio')
        Float('sparkAngle', self, INPUT, units='deg', default=-37.0,
              doc = 'Spark Angle with respect to TDC (Top Dead Center)')
        Int('nCyl', self, INPUT, default=6,
            doc = 'Number of Cylinders')
        Float('IVO', self, INPUT, units='deg', default=11.0,
              doc = 'Intake Valve Open before TDC (Top Dead Center)')
        Float('IVC', self, INPUT, units='deg', default=53.0,
              doc = 'Intake Valve Open after BDC (Bottom Dead Center)')
        Float('Liv', self, INPUT, units='mm', default=8.0, 
              doc='Maximum Valve Lift')
        Float('Div', self, INPUT, units='mm', default=41.2, 
              doc='Inlet Valve Diameter')

        # Promoted From Transmission
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
        Float('FinalDriveRatio', self, INPUT, units=None, default=2.8,
              doc='Final Drive Ratio')
        Float('TireCirc', self, INPUT, units='inch', default=75.0,
              doc='Circumference of tire (inches)')

        # Promoted From Vehicle_Dynamics
        Float('Mass_Vehicle', self, INPUT, units='kg', default=1200.0,
              doc='Vehicle Mass')
        Float('Cf', self, INPUT, units=None, default=0.01,
              doc='Friction Coefficient (proportional to W)')
        Float('Cd', self, INPUT, units=None, default=0.3,
              doc='Drag Coefficient (proportional to V**2)')
        Float('Area', self, INPUT, units='m**2', default=2.164,
              doc='Frontal area')
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
        Float('Acceleration', self, OUTPUT, units='m/s', default=0.0, 
              doc='Calculated vehicle acceleration ')        
        
        # Hook it all up
        
        self.connect('Transmission.RPM','Engine.RPM')
        self.connect('Transmission.TorqueRatio','VDyn.Torque_Ratio')
        self.connect('Engine.Torque','VDyn.Engine_Torque')
        self.connect('Engine.EngineWeight','VDyn.Mass_Engine')

        #self.connect('CurrentGear','Transmission.CurrentGear')
        #self.connect('Velocity','Transmission.Velocity')
        #self.connect('Throttle','Engine.Throttle')
        #self.connect('Velocity','VDyn.Velocity')
        #self.connect('Engine.Power','Power')
        #self.connect('Engine.Torque','Torque')
        #self.connect('Engine.FuelBurn','FuelBurn')
        #self.connect('VDyn.Acceleration','Acceleration')

        
    def execute(self):
        
        self.set('Engine.stroke', self.stroke)        
        self.set('Engine.bore', self.bore)        
        self.set('Engine.conrod', self.conrod)        
        self.set('Engine.compRatio', self.compRatio)        
        self.set('Engine.sparkAngle', self.sparkAngle)        
        self.set('Engine.nCyl', self.nCyl)        
        self.set('Engine.IVO', self.IVO)        
        self.set('Engine.IVC', self.IVC)        
        self.set('Engine.Liv', self.Liv)        
        self.set('Engine.Div', self.Div)        

        self.set('Transmission.Ratio1', self.Ratio1)        
        self.set('Transmission.Ratio2', self.Ratio2)        
        self.set('Transmission.Ratio3', self.Ratio3)        
        self.set('Transmission.Ratio4', self.Ratio4)        
        self.set('Transmission.Ratio5', self.Ratio5)        
        self.set('Transmission.FinalDriveRatio', self.FinalDriveRatio)        
        self.set('Transmission.TireCirc', self.TireCirc)        

        self.set('VDyn.Mass_Vehicle', self.Mass_Vehicle)        
        self.set('VDyn.Cf', self.Cf)        
        self.set('VDyn.Cd', self.Cd)        
        self.set('VDyn.Area', self.Area)        
        #self.set('Transmission.TireCirc', self.TireCirc)        

        self.set('Transmission.CurrentGear', self.CurrentGear)        
        self.set('Transmission.Velocity', self.Velocity*60.0/26.8224)        
        self.set('Engine.Throttle', self.Throttle)        
        self.set('VDyn.Velocity', self.Velocity)        
        #self.VDyn.setvar('Velocity', self.getvar("Velocity"))
        
        super(Vehicle, self).execute()
        
        self.Power =self.get('Engine.Power')    
        self.Torque = self.get('Engine.Torque')    
        self.FuelBurn = self.get('Engine.FuelBurn')      
        self.Acceleration = self.get('VDyn.Acceleration') 
        
        return RUN_OK

        
if __name__ == "__main__":        
    z = Vehicle("Testing")        
    z.CurrentGear = 1
    z.Velocity = 60.0*(26.8224/60.0)
    z.Throttle = .2
    z.execute()
    
    def prz(zz):
        print "Accel = ", zz.Acceleration
        print "Fuelburn = ", zz.FuelBurn
        print "(Power, Torque) ", zz.Power, zz.Torque
        print "RPM = ", zz.Engine.RPM
        
    prz(z)

# End vehicle.py 