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
        
        Int('CurrentGear', self, INPUT, default=0,
              doc='Gear Ratio in Fifth Gear')
        Float('Velocity', self, INPUT, units='mi/h', default=0.0,
              doc='Current Velocity of Vehicle')
        Float('Throttle', self, INPUT, units=None, default=1.0, min_limit=0.1,
              max_limit=1.0, doc='Throttle position (from low idle to wide open)')

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
        
        self.set('Transmission.CurrentGear', self.CurrentGear)        
        self.set('Transmission.Velocity', self.Velocity)        
        self.set('Engine.Throttle', self.Throttle)        
        self.set('VDyn.Velocity', self.Velocity)        
        
        super(Vehicle, self).execute()
        
        self.Power =self.get('Engine.Power')    
        self.Torque = self.get('Engine.Torque')    
        self.FuelBurn = self.get('Engine.FuelBurn')      
        self.Acceleration = self.get('VDyn.Acceleration') 
        

        
if __name__ == "__main__":        
    z = Vehicle("Testing")        
    z.CurrentGear = 1
    z.Velocity = 20.0
    z.Throttle = 1.0
    z.execute()
    print z.Acceleration, z.FuelBurn, z.Power, z.Torque

# End vehicle.py 