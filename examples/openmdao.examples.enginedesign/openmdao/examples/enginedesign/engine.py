"""
    engine.py - Engine (Python implementation) for the vehicle example problem.
"""

# This openMDAO component contains an engine model found in Sitthiracha "AN
# ANALYTICAL MODEL OF SPARK IGNITION ENGINE FOR PERFORMANCE PREDICTION"

from math import pi, sin, cos, exp

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.lib.api import Float, Int

class Engine(Component):
    """ Model of a piston engine - Python Implementation."""
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    # "Instance of <class> has no <attr> member"        
    stroke = Float(78.8, iotype='in', units='mm', desc='Cylinder Stroke')
    bore = Float(82.0, iotype='in', units='mm',  desc='Cylinder Bore')
    conrod = Float(115.0, iotype='in', units='mm',
                   desc='Connecting Rod Length')
    comp_ratio = Float(9.3, iotype='in', desc='Compression Ratio')
    spark_angle = Float(-37.0, iotype='in', units='deg',
                    desc = 'Spark Angle with respect to TDC (Top Dead Center)')
    ncyl = Int(6, iotype='in', desc = 'Number of Cylinders')
    IVO = Float(11.0, iotype='in', units='deg',
                     desc = 'Intake Valve Open before TDC (Top Dead Center)')
    IVC = Float(53.0, iotype='in', units='deg', 
                     desc = 'Intake Valve Open after BDC (Bottom Dead Center)')
    L_v = Float(8.0, iotype='in', units='mm',
                     desc='Maximum Valve Lift')
    D_v = Float(41.2, iotype='in', units='mm',
                     desc='Inlet Valve Diameter')

    RPM = Float(1000.0, low=1000., high=6000., iotype='in', 
                     units='1/min',  desc='Engine RPM')
    throttle = Float(1.0, low=0.01, high=1.0, iotype='in', 
                     desc='Throttle position (from low idle to wide open)')

    power = Float(0.0, iotype='out', units='kW', 
                       desc='Power at engine output')
    torque = Float(0.0, iotype='out', units='N*m', 
                        desc='Torque at engine output')
    fuel_burn = Float(0.0, iotype='out', units='l/s', 
                           desc='Fuel Burn Rate')
    engine_weight = Float(0.0, iotype='out', units='kg', 
                               desc='Engine weight estimation')
        
    #def __init__(self, desc=None, directory=''):
        #""" Creates a new Engine object

            ## Design parameters
            #stroke = 78.8           # Stroke (mm)
            #bore = 82.0             # Bore (mm)
            #conrod = 115.0          # Connecting Rod (mm)
            #comp_ratio = 9.3        # Compression Ratio
            #spark_angle = -37.0     # Spark Angle ref TDC (degree)
            #ncyl = 6                # Number of Cylinders
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
        stroke = self.stroke*.001
        bore = self.bore*.001
        conrod = self.conrod*.001
        D_v = self.D_v*.001
        L_v = self.L_v*.001
        comp_ratio = self.comp_ratio
        spark_angle = self.spark_angle*pi/180.0
        ncyl = self.ncyl
        IVO = self.IVO
        IVC = self.IVC
        RPM = self.RPM

        #--------------------------------------------------------------
        # Calculations independent of crank angle
        #--------------------------------------------------------------

        disp = .25*pi*bore*bore*stroke*ncyl
        l_a = conrod/(.5*stroke)          # a=half the stroke
        resVol = 1.0/(comp_ratio-1.0)
        n = RPM*.001
        t_to_theta = RPM/60.0*2.0*pi
        thetastep *= pi/180.0
        intake_close = (IVC-180.0)*pi/180.0
        intake_open = (IVO-360.0)*pi/180.0

        # Burn duration valid for speeds between 1000 and 6000 RPM (Eq 3-6)
        # Burn end taken at dQ/dt = 1e-15 (very conservative)
        burn_duration = (-1.6189*n*n + 19.886*n + 39.951)*pi/180.0
        burn_end = 2.0*burn_duration
        r_burn_duration = 1.0/burn_duration

        # Exhaust Temperature valid for speeds between 1000 and 6000 RPM \
        # (Eq 3-21)
        T_exh = 3.3955*n*n*n - 51.9*n*n + 279.49*n + 676.21

        # Residual Mass
        # Exhaust gas (mw = 30.4 g/mol, P = 1.52 atm)
        m_res = 1.52*(101.325)*30.4*disp/((comp_ratio-1.0)*T_exh*Ru)

        # Mean Piston Speed
        Cm = 2*stroke*RPM/60.0

        # Frictional Loss Factor valid for speeds between 1000 and 6000 RPM \
        # (Eq 3-19)
        Cf = -0.019738*n + 0.986923

        # Charge Heating Factor valid for speeds between 1000 and 6000 RPM \
        # (Eq 3-20)
        C_heat = -0.043624*n + 1.2953

        # Pressure ratio for choked flow in intake valve
        Pratio_crit = (2/(k+1))**(k/(k-1))

        # Fuel-Air Molecular Weight
        #mw = (1.0 + AFR)/(AFR*mw_air + 1.0/mw_fuel)
        mw = (AFR*mw_air + mw_fuel)/(1.0+AFR)

        #Hohenberg Correlation: crank-angle independent portion
        h_ind = 130.0 * disp**(-0.06) * (Cm+1.4)**0.8

        # FMEP (frictional Losses) (Eq 3-22)
        FMEP = .05*n*n + .15*n + .97

        # Correct ambient P & T for losses
        P0 = P_amb*Cf
        T0 = T_amb*C_heat
        
        # Orifice equation constant terms
        C1 = (2000.0*mw/(Ru*T0) * (k/(k-1)))
        C2 = thetastep*self.throttle*P0/t_to_theta
        e1 = 2.0/k
        e2 = (k + 1.0)/k
        
        # Heat Input Eq Constant Term
        Qfac = .95*Hu/(1.0+AFR)
        
        # Valve Lifting function constant terms
        valve1 = pi/( IVO + IVC + 180 )

        # Initial value for all integration variables (and their dependents)
        mass_in = 0.0
        Qloss = 0.0
        P = P_exth
        Pmix = 0.0
        pmi = 0.0

        for thetad in xrange(-360, 181):
            theta = thetad*thetastep

            #--------------------------------------------------------------
            # Slider Crank Model
            #--------------------------------------------------------------
            
            s_theta = sin(theta)
            c_theta = cos(theta)
            term = (l_a**2 - s_theta**2)**.5
            term2 = (l_a + 1.0 - c_theta - term) 

            # Clyinder Volume (Eq 3-1)
            V = disp*( resVol + .5*term2 )
            dV_dtheta = .5*disp*s_theta*( 1.0 + c_theta/term )

            # Exposed Combustion Area (Eq 3-2)
            A = .5*pi*bore*( bore + stroke*term2 )

            #--------------------------------------------------------------
            # Weibe Function
            #--------------------------------------------------------------

            thetaSinceSpark = theta - spark_angle

            if thetaSinceSpark > 0 and thetaSinceSpark < burn_end:

                # Weibe Function for mass fraction burn (Eq 3-4)
                # weibe = 1.0 - exp(-5.0*pow(thetaSinceSpark/burn_duration, 3))
                fac1 = thetaSinceSpark*r_burn_duration
                dWeibe_dtheta = -exp(-5.0*(fac1)**3.0)*(
                    -15.0*(fac1)**2.0)*r_burn_duration

                #--------------------------------------------------------------
                # Calculate Total Heat Input
                #--------------------------------------------------------------
    
                # Total Heat Input. (Eq 3-7)
                # Mass_in is integrated as we go from IVO to IVC 
                # .95 because not all mass is burned.
                Q = Qfac*mass_in
    
                #--------------------------------------------------------------
                # Calculate Heat Release
                #--------------------------------------------------------------
    
                # Heat Release. (Eq 3-5)
                dQ_dtheta = Q*dWeibe_dtheta
                
            else:
                dQ_dtheta = 0.0

            #--------------------------------------------------------------
            # Cylinder Pressure Model
            #--------------------------------------------------------------

            # Cylinder Pressure. (Eq 3-3)
            P += (((k-1)*(dQ_dtheta - Qloss) - k*P*dV_dtheta)/V)*thetastep

            # Calculate mass flow only when intake valve is open
            if theta <= intake_close and theta >= intake_open:

                #--------------------------------------------------------------
                # Valve Lift, Area, and Discharge Coefficient
                #--------------------------------------------------------------

                phi = valve1*( IVO - IVC + 540 + 2.0*thetad )

                # Valve Lift Function. (Eq 3-16)
                Lv = .5*L_v*(1+cos(phi))

                # Valve curtain area. (Eq 3-12)
                Ar = pi*D_v*Lv
    
                LD = Lv/D_v
    
                # Discharge coefficient for inlet poppet valve. (Eq 3-18)
                CD = ( 190.47*LD*LD*LD*LD - 143.13*LD*LD*LD +
                       31.248*LD*LD - 2.5999*LD + 0.6913 )
                
                #--------------------------------------------------------------
                # Find pressure ratio for intake flow
                #--------------------------------------------------------------
    
                # Note 5.5 is a fudge factor that still needs investigation.
                Pratio = (P+5.5*Pmix)/P0

                # Pratio>1 means outflow
                if Pratio > 1:
                    Pratio = 1.0/Pratio
                    flow_direction = -1.0
                else:
                    flow_direction = 1.0

                if Pratio < Pratio_crit:
                    Pratio = Pratio_crit
                
                #--------------------------------------------------------------
                # Calculate Intake Mass Flow
                #--------------------------------------------------------------
    
                # Mass flow rate. (Eq 3-15)
                # Note, 3-15 is wrong, or an approximation or something
                # Changed to standard orifice equation for better results
                dm_dtheta = flow_direction*CD*Ar*C2*( C1*
                        (Pratio**e1 - Pratio**e2) )**.5
                mass_in += dm_dtheta


            #--------------------------------------------------------------
            # Fuel Stochiometry
            #--------------------------------------------------------------

            # Temperature
            Tg = P*V*mw/((mass_in+m_res)*Ru)

            # Mixture Pressure (kPa)
            Pmix = mass_in*T0*Ru/(mw*V)

            #--------------------------------------------------------------
            # Calculate Heat Transfer Coefficient
            #--------------------------------------------------------------

            # Hohenberg Correlation. (Eq 3-10)
            h = h_ind * P**0.8 * Tg**(-0.4)

            #--------------------------------------------------------------
            # Calculate Heat Loss
            #--------------------------------------------------------------

            # Heat loss to the Cylinder Wall (Kj/radian). (Eq 3-8)
            Qloss = .001*h*A*(Tg-Tw)/t_to_theta

            #--------------------------------------------------------------
            # Work & Power
            #--------------------------------------------------------------

            # IMEP (Eq 3-23)
            pmi += (P+Pmix)*dV_dtheta


        # Effective Pressure (Eq 3-24)
        BMEP = pmi*thetastep/disp - FMEP

        # Effective Power (kwatt=kN*m/sec) (Eq 3-25)
        self.power = 0.5*BMEP*RPM*disp*ncyl/60

        # Torque (kN*m->Nm) (Eq 3-26)
        self.torque = 30.0*self.power/(pi*RPM)*1000.0

        # Fuel Burn (liters/sec)
        self.fuel_burn = (ncyl*mass_in*1000.0*RPM)/(
            60.0*fuel_density*(1.0+AFR)*2.0)

        # Engine Wieght (Empirical) (kg)
        self.engine_weight = (100.0/.002)*(disp-.001) + 75.0

# end engine.py        

if __name__ == "__main__": 
    
    MyEngine = Engine("Testing")
    
    import time
    ttime = time.time()
    
    for jj in xrange(1, 50):
        MyEngine.run()
        
    print "Elapsed time: ", time.time()-ttime

