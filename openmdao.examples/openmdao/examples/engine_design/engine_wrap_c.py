# engine_wrap_c.py
#
# This openMDAO component contains an engine model found in Sitthiracha "AN
# ANALYTICAL MODEL OF SPARK IGNITION ENGINE FOR PERFORMANCE PREDICTION"
#
# This version of the component wraps the C implementation of the model

from openmdao.main import Component, Float, Int
from openmdao.main.variable import INPUT, OUTPUT
from math import pi, sin, cos, exp

from openmdao.examples.engine_design.engineC import RunEngineCycle

class Engine(Component):
    def __init__(self, name, parent=None, doc=None, directory=''):
        ''' Creates a new Engine object

            # Design parameters
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

            # Constants
            k = 1.3                    # k (Specific heat ratio for Air)
            R = 287.0                  # R (Gas constant for Air - J/kg/degK)
            Ru = 8.314                 # R (Gas constant for Air - J/mole/degK)
            Hu = 44000.0               # Heating Value for gasoline (44000 kJ/kg)
            Tw = 400.0                 # Tw (Combustion Wall Temperature 400 degrees K)
            AFR = 14.6                 # Air Fuel Ratio for gasoline
            Pexth = 152                # Exhaust gas pressure
            Pamb = 101.325             # Ambient Pressure (kPa)
            Tamb = 298                 # Ambient Temperature (deg K)
            Air_Density = 1.2          # Air Density (1.2 kg/m**2)
            MwAir = 28.97              # Molecular Weight of Air (g/mol)
            MwFuel = 114               # Molecular Weight of Gasoline (g/mol)

            # Simulation inputs
            RPM = 1000.0               # RPM
            throttle = 1.0             # Throttle Position
            thetastep = 1.0            # Simulation time stepsize (crank angle degrees)

            # Outputs
            Power                      # Power at engine output (KW)
            Torque                     # Torque at engine output (N*m)
            FuelBurn                   # Fuel burn rate (liters/sec)
            EngineWeight               # Engine weight estimation (kg)
            '''

        super(Engine, self).__init__(name, parent, doc, directory)        

        # set up interface to the framework  
        # pylint: disable-msg=E1101
        # "Instance of <class> has no <attr> member"        
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

        Float('RPM', self, INPUT, units='1/min', default=1000.0, min_limit=1000,
              max_limit=6000, doc='Engine RPM')
        Float('Throttle', self, INPUT, units=None, default=1.0, min_limit=0.01,
              max_limit=1.0, doc='Throttle position (from low idle to wide open)')

        Float('Power', self, OUTPUT, units='kW', default=0.0,
              doc='Power at engine output')
        Float('Torque', self, OUTPUT, units='N*m', default=0.0,
              doc='Torque at engine output')
        Float('FuelBurn', self, OUTPUT, units='l/s', default=0.0,
              doc='Torque at engine output')
        Float('EngineWeight', self, OUTPUT, units='kg', default=0.0,
              doc='Engine weight estimation')


    def execute(self):
        ''' Simulates the Otto cycle for an internal combustion engine.
            Power and Torque are returned at the engine output.
            '''

        # These Constants are all hard-coded for Gasoline.
        # Eventually, we'll move them to the input so that they can be tweaked.
        # (Possibly by just selecting a fuel-type)
        k = 1.3                    # k (Specific heat ratio for Air)
        R = 287.0                  # R (Gas constant for Air - J/kg/degK)
        Ru = 8.314                 # R (Gas constant for Air - J/mole/degK)
        Hu = 44000.0               # Heating Value for gasoline (44000 kJ/kg)
        Tw = 400.0                 # Tw (Combustion Wall Temperature 400 degrees K)
        AFR = 14.6                 # Air Fuel Ratio for gasoline
        Pexth = 152                # Exhaust gas pressure
        Pamb = 101.325             # Ambient Pressure (kPa)
        Tamb = 298                 # Ambient Temperature (deg K)
        Air_Density = 1.2          # Air Density (1.2 kg/m**2)
        Fuel_Density = 740.0       # Gasoline Density (740.0 kg/m**2)
        MwAir = 28.97              # Molecular Weight of Air (g/mol)
        MwFuel = 114               # Molecular Weight of Gasoline (g/mol)

        thetastep = 1.0            # Simulation time stepsize (crank angle degrees)

        # Convert mm to m
        stroke = self.stroke*.001
        bore = self.bore*.001
        conrod = self.conrod*.001
        Div = self.Div*.001
        Liv = self.Liv*.001
        compRatio = self.compRatio
        sparkAngle = self.sparkAngle*pi/180.0
        nCyl = self.nCyl
        IVO = self.IVO
        IVC = self.IVC
        RPM = self.RPM
        Throttle = self.Throttle
        
        Power = 0.0
        Torque = 0.0
        FuelBurn = 0.0
        EngineWeight = 0.0
        
        # Call the C model and pass it what it needs.
        
        Power, Torque, FuelBurn, EngineWeight = RunEngineCycle(
                    stroke, bore, conrod, compRatio, sparkAngle,
                    nCyl, IVO, IVC, Liv, Div, k,
                    R, Ru, Hu, Tw, AFR, Pexth,
                    Tamb, Pamb, Air_Density, MwAir, MwFuel,
                    RPM, Throttle, thetastep, Fuel_Density)

        
        # Interogate results of engine simulation and store.
        
        self.Power = Power[0]
        self.Torque = Torque[0]
        self.FuelBurn = FuelBurn[0]
        self.EngineWeight = EngineWeight[0]

# end engine.py        

if __name__ == "__main__": 
    
    z = Engine("Testing")
    
    import time
    t1 = time.time()
    
    for jj in xrange(1,500):
        z.run()

    print z.Throttle, z.Power, z.Torque, z.FuelBurn, z.EngineWeight
        
    print "Elapsed time: ", time.time()-t1

