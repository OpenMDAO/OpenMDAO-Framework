# engine_wrap_c.py
#
# This openMDAO component contains an engine model found in Sitthiracha "AN
# ANALYTICAL MODEL OF SPARK IGNITION ENGINE FOR PERFORMANCE PREDICTION"
#
# This version of the component wraps the C implementation of the model

from math import pi

from openmdao.main import Component, Float, Int
from openmdao.main.variable import INPUT, OUTPUT

from openmdao.examples.engine_design.engineC import RunEngineCycle

class Engine(Component):
    ''' Model of a piston engine - C Implementation.'''
    
    def __init__(self, name, parent=None, doc=None, directory=''):
        ''' Creates a new Engine object

            # Design parameters
            stroke = 78.8              # Stroke (mm)
            bore = 82.0                # Bore (mm)
            conrod = 115.0             # Connecting Rod (mm)
            comp_ratio = 9.3           # Compression Ratio
            spark_angle = -37.0        # Spark Angle ref TDC (degree)
            n_cyl = 6                  # Number of Cylinders
            IVO = 11.0                 # Intake Valve Open before TDC (degree BTDC)
            IVC = 53.0                 # Intake Valve Close after BDC (degree ABDC)
            L_v = 8.0                  # Maximum Valve Lift (mm)
            D_v = 41.2                 # Inlet Valve Dia (mm)

            # Constants
            k = 1.3                    # k (Specific heat ratio for Air)
            R = 287.0                  # R (Gas constant for Air - J/kg/degK)
            Ru = 8.314                 # R (Gas constant for Air - J/mole/degK)
            Hu = 44000.0               # Heating Value for gasoline (44000 kJ/kg)
            Tw = 400.0                 # Tw (Combustion Wall Temperature 400 degrees K)
            AFR = 14.6                 # Air Fuel Ratio for gasoline
            P_exth = 152               # Exhaust gas pressure
            P_amb = 101.325            # Ambient Pressure (kPa)
            T_amb = 298                # Ambient Temperature (deg K)
            air_density = 1.2          # Air Density (1.2 kg/m**2)
            mw_air = 28.97             # Molecular Weight of Air (g/mol)
            mw_fuel = 114              # Molecular Weight of Gasoline (g/mol)

            # Simulation inputs
            RPM = 1000.0               # RPM
            throttle = 1.0             # Throttle Position
            thetastep = 1.0            # Simulation time stepsize (crank angle degrees)

            # Outputs
            power                      # Power at engine output (KW)
            torque                     # Torque at engine output (N*m)
            fuel_burn                  # Fuel burn rate (liters/sec)
            engine_weight              # Engine weight estimation (kg)
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
        Float('comp_ratio', self, INPUT, units=None, default=9.3, 
              doc='Compression Ratio')
        Float('spark_angle', self, INPUT, units='deg', default=-37.0,
              doc = 'Spark Angle with respect to TDC (Top Dead Center)')
        Int('n_cyl', self, INPUT, default=6,
            doc = 'Number of Cylinders')
        Float('IVO', self, INPUT, units='deg', default=11.0,
              doc = 'Intake Valve Open before TDC (Top Dead Center)')
        Float('IVC', self, INPUT, units='deg', default=53.0,
              doc = 'Intake Valve Open after BDC (Bottom Dead Center)')
        Float('L_v', self, INPUT, units='mm', default=8.0, 
              doc='Maximum Valve Lift')
        Float('D_v', self, INPUT, units='mm', default=41.2, 
              doc='Inlet Valve Diameter')

        Float('RPM', self, INPUT, units='1/min', default=1000.0, min_limit=1000,
              max_limit=6000, doc='Engine RPM')
        Float('throttle', self, INPUT, units=None, default=1.0, min_limit=0.01,
              max_limit=1.0, doc='Throttle position \
              (from low idle to wide open)')

        Float('power', self, OUTPUT, units='kW', default=0.0,
              doc='Power at engine output')
        Float('torque', self, OUTPUT, units='N*m', default=0.0,
              doc='Torque at engine output')
        Float('fuel_burn', self, OUTPUT, units='l/s', default=0.0,
              doc='Fuel Burn Rate')
        Float('engine_weight', self, OUTPUT, units='kg', default=0.0,
              doc='Engine weight estimation')


    def execute(self):
        ''' Simulates the Otto cycle for an internal combustion engine.
            Power and Torque are returned at the engine output.
            '''

        # These Constants are all hard-coded for Gasoline.
        # Eventually, we'll move them to the input so that they can be tweaked.
        # (Possibly by just selecting a fuel-type)
        k = 1.3                # k (Specific heat ratio for Air)
        R = 287.0              # R (Gas constant for Air - J/kg/degK)
        Ru = 8.314             # R (Gas constant for Air - J/mole/degK)
        Hu = 44000.0           # Heating Value for gasoline (44000 kJ/kg)
        Tw = 400.0             # Tw (Combustion Wall Temperature 400 degrees K)
        AFR = 14.6             # Air Fuel Ratio for gasoline
        P_exth = 152           # Exhaust gas pressure
        P_amb = 101.325        # Ambient Pressure (kPa)
        T_amb = 298            # Ambient Temperature (deg K)
        air_density = 1.2      # Air Density (1.2 kg/m**2)
        fuel_density = 740.0   # Gasoline Density (740.0 kg/m**2)
        mw_air = 28.97         # Molecular Weight of Air (g/mol)
        mw_fuel = 114          # Molecular Weight of Gasoline (g/mol)
        thetastep = 1.0        # Simulation time stepsize (crank angle degrees)

        # Convert mm to m
        stroke = self.stroke*.001
        bore = self.bore*.001
        conrod = self.conrod*.001
        D_v = self.D_v*.001
        L_v = self.L_v*.001
        comp_ratio = self.comp_ratio
        spark_angle = self.spark_angle*pi/180.0
        n_cyl = self.n_cyl
        IVO = self.IVO
        IVC = self.IVC
        RPM = self.RPM
        throttle = self.throttle
        
        power = 0.0
        torque = 0.0
        fuel_burn = 0.0
        engine_weight = 0.0
        
        # Call the C model and pass it what it needs.
        
        power, torque, fuel_burn, engine_weight = RunEngineCycle(
                    stroke, bore, conrod, comp_ratio, spark_angle,
                    n_cyl, IVO, IVC, L_v, D_v, k,
                    R, Ru, Hu, Tw, AFR, P_exth,
                    T_amb, P_amb, air_density, mw_air, mw_fuel,
                    RPM, throttle, thetastep, fuel_density)

        
        # Interogate results of engine simulation and store.
        
        self.power = power[0]
        self.torque = torque[0]
        self.fuel_burn = fuel_burn[0]
        self.engine_weight = engine_weight[0]

# end engine.py        

if __name__ == "__main__": 
    
    z = Engine("Testing")
    
    import time
    t1 = time.time()
    
    for jj in xrange(1, 500):
        z.run()

    print z.throttle, z.power, z.torque, z.fuel_burn, z.engine_weight
        
    print "Elapsed time: ", time.time()-t1

