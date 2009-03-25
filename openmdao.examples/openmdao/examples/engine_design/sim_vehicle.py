# sim_vehicle.py
#
# Simulates a vehicle to obatain the following.
# Vehicle assembly fits in the socket.

from openmdao.main.component import Component, RUN_OK
from openmdao.main import Float, Int
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.exceptions import ConstraintError

from openmdao.examples.engine_design.vehicle import Vehicle

class Sim_Vehicle(Component):
    def __init__(self, name, parent=None, doc=None, directory=''):
        ''' Creates a new Sim_Accel object
        
            # Simulation inputs
            EndSpeed           # Simulation ending speed in mph.
            TimeStep           # Simulation time step in sec.
            
            # Outputs
            AccelTime         # Time to reach 60 mph from start
            '''
        
        super(Sim_Vehicle, self).__init__(name, parent, doc, directory)    

        # set up interface to the framework  
        # Pylint: disable-msg=E1101

        # Promoted From Vehicle -> Engine
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

        # Promoted From Vehicle -> Transmission
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

        # Promoted From Vehicle -> Vehicle_Dynamics
        Float('Mass_Vehicle', self, INPUT, units='kg', default=1200.0,
              doc='Vehicle Mass')
        Float('Cf', self, INPUT, units=None, default=0.01,
              doc='Friction Coefficient (proportional to W)')
        Float('Cd', self, INPUT, units=None, default=0.3,
              doc='Drag Coefficient (proportional to V**2)')
        Float('Area', self, INPUT, units='m**2', default=2.164,
              doc='Frontal area')

        # Simulation Parameters
        Float('EndSpeed', self, INPUT, units='m/h', default=60.0,
              doc='Simulation final speed')
        Float('TimeStep', self, INPUT, units='s', default=0.1,
              doc='Simulation final speed')
        
        # Outputs
        Float('AccelTime', self, OUTPUT, units='s', default=0.0, 
              doc='Time to reach Endspeed starting from rest')
        
        # NOTE: This stuff will be replaced with Sockets when implemented
        self.vehicle = Vehicle("Test_Vehicle")
        #self.vehicle = None
        
    def execute(self):
        ''' Simulate the vehicle model at full throttle.
            '''
        
        # Promote all design variables from the Vehicle assembly to Sim_Vehicle
        # syntax here is temporary until socket interfaces are implemted.
        
        self.vehicle.stroke = self.stroke        
        self.vehicle.bore = self.bore        
        self.vehicle.conrod = self.conrod        
        self.vehicle.compRatio = self.compRatio        
        self.vehicle.sparkAngle = self.sparkAngle        
        self.vehicle.nCyl = self.nCyl        
        self.vehicle.IVO = self.IVO        
        self.vehicle.IVC = self.IVC        
        self.vehicle.Liv = self.Liv        
        self.vehicle.Div = self.Div        

        self.vehicle.Ratio1 = self.Ratio1        
        self.vehicle.Ratio2 = self.Ratio2        
        self.vehicle.Ratio3 = self.Ratio3        
        self.vehicle.Ratio4 = self.Ratio4        
        self.vehicle.Ratio5 = self.Ratio5        
        self.vehicle.FinalDriveRatio = self.FinalDriveRatio        
        self.vehicle.TireCirc = self.TireCirc        

        self.vehicle.Mass_Vehicle = self.Mass_Vehicle        
        self.vehicle.Cf = self.Cf        
        self.vehicle.Cd = self.Cd        
        self.vehicle.Area = self.Area        

        #--------------------------------------------------------------------
        # Simulate acceleration time from 0 to EndSpeed
        #--------------------------------------------------------------------
        
        Velocity = 0.0
        Time = 0.0
        
        # Set throttle and gear
        self.vehicle.CurrentGear = 1
        self.vehicle.Throttle = 1.0
        self.vehicle.Velocity = 0.0
            
        while Velocity < self.EndSpeed:
            
            # Find acceleration.
            # If RPM goes over MAX RPM, shift gears
            # (i.e.: shift at redline)
            try:
                self.vehicle.execute()
            except ConstraintError:
                self.vehicle.CurrentGear += 1
                self.vehicle.execute()

            # Accleration converted to mph/s
            Acceleration = self.vehicle.Acceleration*2.23693629
                
            Velocity += Acceleration*self.TimeStep
            self.vehicle.Velocity = Velocity/2.23693629
        
            Time += self.TimeStep
            print Time, self.vehicle.CurrentGear, Velocity, self.vehicle.Transmission.RPM, self.vehicle.Engine.RPM
        
        self.AccelTime = Time
            
        return RUN_OK
        
    
def test_it():    
    z = Sim_Vehicle("New")  
    z.vehicle = Vehicle("Test_Vehicle")
    z.execute()
    print "Gear:", z.vehicle.CurrentGear
    print "Time:", z.AccelTime
    
if __name__ == "__main__": 
    test_it()

# End sim_vehicle.py        
