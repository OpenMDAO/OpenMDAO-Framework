# engine.py
#
# This openMDAO component contains an engine model found in Sitthiracha "AN
# ANALYTICAL MODEL OF SPARK IGNITION ENGINE FOR PERFORMANCE PREDICTION"

from openmdao.main.component import Component, RUN_OK
from openmdao.main import Float, Int
from openmdao.main.variable import INPUT, OUTPUT
from math import pi, sin, cos, exp

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

        #--------------------------------------------------------------
        # Calculations independent of crank angle
        #--------------------------------------------------------------

        disp = .25*pi*bore*bore*stroke*nCyl
        l_a = conrod/(.5*stroke)          # a=half the stroke
        resVol = 1.0/(compRatio-1.0)
        n = RPM*.001
        t_to_theta = RPM/60.0*2.0*pi
        thetastep *= pi/180.0
        intake_close = (IVC-180.0)*pi/180.0
        intake_open = (IVO-360.0)*pi/180.0

        # Burn duration valid for speeds between 1000 and 6000 RPM (Eq 3-6)
        # Burn end taken at dQ/dt = 1e-15 (very conservative)
        burnDuration = (-1.6189*n*n + 19.886*n + 39.951)*pi/180.0
        burnEnd = 2.0*burnDuration
        r_burnDuration = 1.0/burnDuration

        # Exhaust Temperature valid for speeds between 1000 and 6000 RPM (Eq 3-21)
        T_exh = 3.3955*n*n*n - 51.9*n*n + 279.49*n + 676.21

        # Residual Mass
        # Exhaust gas (Mw = 30.4 g/mol, P = 1.52 atm)
        m_res = 1.52*(101.325)*30.4*disp/((compRatio-1.0)*T_exh*Ru)

        # Mean Piston Speed
        Cm = 2*stroke*RPM/60.0

        # Frictional Loss Factor valid for speeds between 1000 and 6000 RPM (Eq 3-19)
        Cf = -0.019738*n + 0.986923

        # Charge Heating Factor valid for speeds between 1000 and 6000 RPM (Eq 3-20)
        C_heat = -0.043624*n + 1.2953

        # Pressure ratio for choked flow in intake valve
        Pratio_crit = (2/(k+1))**(k/(k-1))

        # Fuel-Air Molecular Weight
        #Mw = (1.0 + AFR)/(AFR*MwAir + 1.0/MwFuel)
        Mw = (AFR*MwAir + MwFuel)/(1.0+AFR)

        #Hohenberg Correlation: crank-angle independent portion
        h_ind = 130.0 * disp**(-0.06) * (Cm+1.4)**0.8

        # FMEP (frictional Losses) (Eq 3-22)
        FMEP = .05*n*n + .15*n + .97

        # Correct ambient P & T for losses
        P0 = Pamb*Cf
        T0 = Tamb*C_heat
        
        # Orifice equation constant terms
        C1 = (2000.0*Mw/(Ru*T0) * (k/(k-1)))
        C2 = thetastep*self.Throttle*P0/t_to_theta
        e1 = 2.0/k
        e2 = (k + 1.0)/k
        
        # Heat Input Eq Constant Term
        Qfac = .95*Hu/(1.0+AFR)
        
        # Valve Lifting function constant terms
        valve1 = pi/( IVO + IVC + 180 )

        # Initial value for all integration variables (and their dependents)
        mass_in = 0.0
        Qloss = 0.0
        P = Pexth
        Pmix = 0.0
        pmi = 0.0

        for thetad in xrange(-360,181):
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

            thetaSinceSpark = theta - sparkAngle

            if thetaSinceSpark > 0 and thetaSinceSpark < burnEnd:

                # Weibe Function for mass fraction burn (Eq 3-4)
                # weibe = 1.0 - exp( -5.0*pow( thetaSinceSpark/burnDuration, 3 ) )
                dWeibe_dtheta = - exp( -5.0*(thetaSinceSpark*r_burnDuration)**3.0 )*(
                    -15.0*(thetaSinceSpark*r_burnDuration)**2.0)*r_burnDuration

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
                Lv = .5*Liv*(1+cos(phi))

                # Valve curtain area. (Eq 3-12)
                Ar = pi*Div*Lv
    
                LD = Lv/Div
    
                # Discharge coefficient for inlet poppet valve. (Eq 3-18)
                CD = ( 190.47*LD*LD*LD*LD - 143.13*LD*LD*LD +
                       31.248*LD*LD - 2.5999*LD + 0.6913 )
                
                #--------------------------------------------------------------
                # Find pressure ratio for intake flow
                #--------------------------------------------------------------
    
                # Note 5.5 is a fudge factor that still needs investigation.
                Pratio = (P+5.5*Pmix)/P0

                # Pratio>1 means outflow
                if Pratio>1:
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
            Tg = P*V*Mw/((mass_in+m_res)*Ru)

            # Mixture Pressure (kPa)
            Pmix = mass_in*T0*Ru/(Mw*V)

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
        self.Power = 0.5*BMEP*RPM*disp*nCyl/60

        # Torque (kN*m->Nm) (Eq 3-26)
        self.Torque = 30.0*self.Power/(pi*RPM)*1000.0

        # Fuel Burn (liters/sec)
        self.FuelBurn = (nCyl*mass_in*1000.0*RPM)/(60.0*Fuel_Density*(1.0+AFR)*2.0)

        # Engine Wieght (Empirical) (kg)
        self.EngineWeight = (100.0/.002)*(disp-.001) + 75.0

        return RUN_OK

# end engine.py        

if __name__ == "__main__": 
    
    z = Engine("Testing")
    
    import time
    t1 = time.time()
    
    for jj in xrange(1,50):
        z.execute()
        
    print "Elapsed time: ", time.time()-t1