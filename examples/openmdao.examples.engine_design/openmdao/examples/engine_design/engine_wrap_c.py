"""
    engine_wrap.py - Engine (C implementation) for the vehicle example problem.
"""

# This openMDAO component contains an engine model found in Sitthiracha "AN
# ANALYTICAL MODEL OF SPARK IGNITION ENGINE FOR PERFORMANCE PREDICTION"
#
# This version of the component wraps the C implementation of the model

from math import pi

from enthought.traits.api import Float, Int, Range

from openmdao.main.api import Component
from openmdao.lib.traits.unitsfloat import UnitsFloat

from openmdao.examples.engine_design.engineC import RunEngineCycle

class Engine(Component):
    """ Model of a piston engine - C Implementation."""
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    # "Instance of <class> has no <attr> member"        
    stroke = UnitsFloat(78.8, iostatus='in', units='mm',
                        desc='Cylinder Stroke')
    bore = UnitsFloat(82.0, iostatus='in', units='mm', 
                      desc='Cylinder Bore')
    conrod = UnitsFloat(115.0, iostatus='in', units='mm', 
                        desc='Connecting Rod Length')
    comp_ratio = Float(9.3, iostatus='in', units=None, 
                       desc='Compression Ratio')
    spark_angle = UnitsFloat(-37.0, iostatus='in', units='deg', 
                        desc = 'Spark Angle with respect to TDC (Top Dead Center)')
    n_cyl = Int(6, iostatus='in', desc = 'Number of Cylinders')
    IVO = UnitsFloat(11.0, iostatus='in', units='deg', 
                     desc = 'Intake Valve Open before TDC (Top Dead Center)')
    IVC = UnitsFloat(53.0, iostatus='in', units='deg', 
                     descc = 'Intake Valve Open after BDC (Bottom Dead Center)')
    L_v = UnitsFloat(8.0, iostatus='in', units='mm', 
                     desc='Maximum Valve Lift')
    D_v = UnitsFloat(41.2, iostatus='in', units='mm', 
                     desc='Inlet Valve Diameter')

    RPM = UnitsFloat(1000.0, low=1000., high=6000., 
                     iostatus='in', units='1/min', 
                     desc='Engine RPM')
    throttle = Range(low=0.01, high=1.0, value=1.0, iostatus='in', 
                     desc='Throttle position (from low idle to wide open)')

    power = UnitsFloat(0., iostatus='out', units='kW', 
                       desc='Power at engine output')
    torque = UnitsFloat(0., iostatus='out', units='N*m', 
                        desc='Torque at engine output')
    fuel_burn = UnitsFloat(0., iostatus='out', units='l/s',
                           desc='Fuel Burn Rate')
    engine_weight = UnitsFloat(0., iostatus='out', units='kg', 
                               desc='Engine weight estimation')

    #def __init__(self, desc=None, directory=''):
        #""" Creates a new Engine object

            ## Design parameters
            #stroke = 78.8           # Stroke (mm)
            #bore = 82.0             # Bore (mm)
            #conrod = 115.0          # Connecting Rod (mm)
            #comp_ratio = 9.3        # Compression Ratio
            #spark_angle = -37.0     # Spark Angle ref TDC (degree)
            #n_cyl = 6               # Number of Cylinders
            #IVO = 11.0              # Intake Valve Open before TDC (deg BTDC)
            #IVC = 53.0              # Intake Valve Close after BDC (deg ABDC)
            #L_v = 8.0               # Maximum Valve Lift (mm)
            #D_v = 41.2              # Inlet Valve Dia (mm)

            ## Constants
            #k = 1.3                 # k (Specific heat ratio for Air)
            #R = 287.0               # R (Gas constant for Air - J/kg/degK)
            #Ru = 8.314              # R (Gas constant for Air - J/mole/degK)
            #Hu = 44000.0            # Heating Value for gasoline (44000 kJ/kg)
            #Tw = 400.0              # Tw (Combustion Wall Temperature 400 degK)
            #AFR = 14.6              # Air Fuel Ratio for gasoline
            #P_exth = 152            # Exhaust gas pressure
            #P_amb = 101.325         # Ambient Pressure (kPa)
            #T_amb = 298             # Ambient Temperature (deg K)
            #air_density = 1.2       # Air Density (1.2 kg/m**2)
            #mw_air = 28.97          # Molecular Weight of Air (g/mol)
            #mw_fuel = 114           # Molecular Weight of Gasoline (g/mol)

            ## Simulation inputs
            #RPM = 1000.0            # RPM
            #throttle = 1.0          # Throttle Position
            #thetastep = 1.0         # Sim time stepsize (crank angle deg)

            ## Outputs
            #power                   # Power at engine output (KW)
            #torque                  # Torque at engine output (N*m)
            #fuel_burn               # Fuel burn rate (liters/sec)
            #engine_weight           # Engine weight estimation (kg)
            #"""

        #super(Engine, self).__init__(desc, directory)        


    def execute(self):
        """ Simulates the Otto cycle for an internal combustion engine.
            Power and Torque are returned at the engine output.
            """

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
        throttle = self.throttle
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

if __name__ == "__main__": # pragma: no cover    
    
    z = Engine()
    
    import time
    t1 = time.time()
    
    for jj in xrange(1, 500):
        z.run()

    print z.throttle, z.power, z.torque, z.fuel_burn, z.engine_weight
        
    print "Elapsed time: ", time.time()-t1

