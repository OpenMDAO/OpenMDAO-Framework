"""
    engine_wrap.py - Engine (C implementation) for the vehicle example problem.
"""

# This openMDAO component contains an engine model found in Sitthiracha "AN
# ANALYTICAL MODEL OF SPARK IGNITION ENGINE FOR PERFORMANCE PREDICTION"
#
# This version of the component wraps the C implementation of the model

from math import pi

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float, Int, Bool

from openmdao.examples.enginedesign.engineC import RunEngineCycle


class Engine(Component):
    """ Model of a piston engine - C Implementation."""

    # Design parameters:
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

    # Constants:
    #k = 1.3                 # k (Specific heat ratio for Air)
    #R = 287.0               # R (Gas constant for Air - J/kg/degK)
    #Ru = 8.314              # R (Gas constant for Air - J/mole/degK)
    #Hu = 44000.0            # Heating Value for gasoline (44000 kJ/kg)
    #Tw = 400.0              # Tw (Combustion Wall Temperature 400 degK)
    #AFR = 14.6              # Air Fuel Ratio for gasoline
    #P_exth = 152            # Exhaust gas pressure
    #P_amb = 101.325         # Ambient Pressure (kPa)
    #T_amb = 298             # Ambient Temperature (deg K)
    #air_density = 1.2       # Air Density (1.2 kg/m**3)
    #mw_air = 28.97          # Molecular Weight of Air (g/mol)
    #mw_fuel = 114           # Molecular Weight of Gasoline (g/mol)

    # Simulation inputs:
    #RPM = 1000.0            # RPM
    #throttle = 1.0          # Throttle Position
    #thetastep = 1.0         # Sim time stepsize (crank angle deg)

    # Outputs:
    #power                   # Power at engine output (KW)
    #torque                  # Torque at engine output (N*m)
    #fuel_burn               # Fuel burn rate (liters/sec)
    #engine_weight           # Engine weight estimation (kg)
    
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    # "Instance of <class> has no <attr> member"        
    stroke = Float(78.8, iotype='in', units='mm',
                        desc='Cylinder Stroke')
    bore = Float(82.0, iotype='in', units='mm', 
                      desc='Cylinder Bore')
    conrod = Float(115.0, iotype='in', units='mm', 
                        desc='Connecting Rod Length')
    comp_ratio = Float(9.3, iotype='in', 
                       desc='Compression Ratio')
    spark_angle = Float(-37.0, iotype='in', units='deg', 
                    desc = 'Spark Angle with respect to TDC (Top Dead Center)')
    n_cyl = Int(6, iotype='in', desc = 'Number of Cylinders')
    IVO = Float(11.0, iotype='in', units='deg', 
                     desc = 'Intake Valve Open before TDC (Top Dead Center)')
    IVC = Float(53.0, iotype='in', units='deg', 
                     descc = 'Intake Valve Open after BDC (Bottom Dead Center)')
    L_v = Float(8.0, iotype='in', units='mm', 
                     desc='Maximum Valve Lift')
    D_v = Float(41.2, iotype='in', units='mm', 
                     desc='Inlet Valve Diameter')

    RPM = Float(1000.0, iotype='in', units='rpm',  desc='Engine RPM')
    throttle = Float(1.0, low=0.01, high=1.0, iotype='in', 
                     desc='Throttle position (from low idle to wide open)')

    power = Float(0., iotype='out', units='kW', 
                       desc='Power at engine output')
    torque = Float(0., iotype='out', units='N*m', 
                        desc='Torque at engine output')
    fuel_burn = Float(0., iotype='out', units='L/s',
                           desc='Fuel Burn Rate')
    engine_weight = Float(0., iotype='out', units='kg', 
                               desc='Engine weight estimation')
    overspeed = Bool(False, iotype='out', desc='True if the engine RPM '
                            'exceeds its maximum allowable RPM.')
    underspeed = Bool(False, iotype='out', desc='True if the engine RPM '
                            'exceeds its minimum allowable RPM.')


    def execute(self):
        """ Simulates the Otto cycle for an internal combustion engine.
        Power and Torque are returned at the engine output.
        """
        
        # Check engine speed to see if it goes beyond our bounds:
        
        if self.RPM > 6000:
            self.overspeed = True
        else:
            self.overspeed = False

        if self.RPM < 1000:
            self.underspeed = True
        else:
            self.underspeed = False
            
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
        air_density = 1.2      # Air Density (1.2 kg/m**3)
        fuel_density = 740.0   # Gasoline Density (740.0 kg/m**3)
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
        
        (power, torque, fuel_burn, engine_weight) = RunEngineCycle(
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
    
    MyEngine = Engine("Testing")
    
    import time
    start_time = time.time()
    
    for jj in xrange(1, 50):
        MyEngine.run()

    print "\n"
    print 'Throttle Position: ', MyEngine.throttle
    print 'Power: ', MyEngine.power
    print 'Torque: ', MyEngine.torque
    print 'Fuel Burn: ', MyEngine.fuel_burn
    print 'Engine Weight: ', MyEngine.engine_weight
    print '-----------------------------'    
    print "Elapsed time: ", time.time()-start_time, 'seconds'
