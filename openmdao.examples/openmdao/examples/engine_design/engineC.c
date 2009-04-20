/*
 * Engine Simulation

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
 
 */
 
 #include <math.h>
 
 #define PI 3.1415926535
 
 void RunEngineCycle(double stroke, double bore, double conrod, double compRatio, double sparkAngle,
                    int nCyl, double IVO, double IVC, double Liv, double Div, double k,
                    double R, double Ru, double Hu, double Tw, double AFR, double Pexth,
                    double Tamb, double Pamb, double Air_Density, double MwAir, double MwFuel,
                    double RPM, double Throttle, double thetastep, double Fuel_Density,
                    double *Power, double *Torque, double *FuelBurn, double *EngineWeight)
 {
 
    double disp, l_a, resVol, n, t_to_theta, intake_close, intake_open;
    double burnDuration, burnEnd, r_burnDuration, T_exh, m_res, Cm, Cf;
    double C_heat, Pratio_crit, Mw, h_ind, FMEP, P0, T0, C1, C2;
    double e1, e2, Qfac, valve1, mass_in, Qloss, P, Pmix, pmi;
    double dWeibe_dtheta, A, V, dV_dtheta, s_theta, c_theta;
    double term1, term2, Q, dQ_dtheta, phi, Ar, LD, CD, Tg, h, Lv;
    double theta, thetad, thetaSinceSpark, Pratio, flow_direction;
    double BMEP, term, dm_dtheta;

    //--------------------------------------------------------------
    // Calculations independent of crank angle
    //--------------------------------------------------------------

    disp = .25*PI*bore*bore*stroke*nCyl;
    l_a = conrod/(.5*stroke);         
    resVol = 1.0/(compRatio-1.0);
    n = RPM*.001;
    t_to_theta = RPM/60.0*2.0*PI;
    thetastep *= PI/180.0;
    intake_close = (IVC-180.0)*PI/180.0;
    intake_open = (IVO-360.0)*PI/180.0;

    // Burn duration valid for speeds between 1000 and 6000 RPM (Eq 3-6)
    // Burn end taken at dQ/dt = 1e-15 (very conservative)
    burnDuration = (-1.6189*n*n + 19.886*n + 39.951)*PI/180.0;
    burnEnd = 2.0*burnDuration;
    r_burnDuration = 1.0/burnDuration;

    // Exhaust Temperature valid for speeds between 1000 and 6000 RPM (Eq 3-21)
    T_exh = 3.3955*n*n*n - 51.9*n*n + 279.49*n + 676.21;

    // Residual Mass
    // Exhaust gas (Mw = 30.4 g/mol, P = 1.52 atm)
    m_res = 1.52*(101.325)*30.4*disp/((compRatio-1.0)*T_exh*Ru);

    // Mean Piston Speed
    Cm = 2.0*stroke*RPM/60.0;

    // Frictional Loss Factor valid for speeds between 1000 and 6000 RPM (Eq 3-19)
    Cf = -0.019738*n + 0.986923;

    // Charge Heating Factor valid for speeds between 1000 and 6000 RPM (Eq 3-20)
    C_heat = -0.043624*n + 1.2953;

    // Pressure ratio for choked flow in intake valve
    Pratio_crit = pow( 2.0/(k+1.0), k/(k-1.0) );

    // Fuel-Air Molecular Weight
    Mw = (AFR*MwAir + MwFuel)/(1.0+AFR);

    // Hohenberg Correlation: crank-angle independent portion
    h_ind = 130.0 * pow( disp, -0.06 ) * pow( Cm+1.4, 0.8 );

    // FMEP (frictional Losses) (Eq 3-22)
    FMEP = .05*n*n + .15*n + .97;

    // Correct ambient P & T for losses
    P0 = Pamb*Cf;
    T0 = Tamb*C_heat;

    // Orifice equation constant terms
    C1 = (2000.0*Mw/(Ru*T0) * (k/(k-1)));
    C2 = thetastep*Throttle*P0/t_to_theta;
    e1 = 2.0/k;
    e2 = (k + 1.0)/k;

    // Heat Input Eq Constant Term
    Qfac = .95*Hu/(1.0+AFR);
        
    // Valve Lifting function constant terms
    valve1 = PI/( IVO + IVC + 180 );

    // Initial value for all integration variables (and their dependents)
    mass_in = 0.0;
    Qloss = 0.0;
    P = Pexth;
    Pmix = 0.0;
    pmi = 0.0;

    for ( thetad=-360; thetad<181; thetad++ ) {
        
        theta = thetad*thetastep;

        //--------------------------------------------------------------
        // Slider Crank Model
        //--------------------------------------------------------------
    
        s_theta = sin(theta);
        c_theta = cos(theta);
        term = pow( l_a*l_a - s_theta*s_theta, .5 );
        term2 = (l_a + 1.0 - c_theta - term);

        // Clyinder Volume (Eq 3-1)
        V = disp*( resVol + .5*term2 );
        dV_dtheta = .5*disp*s_theta*( 1.0 + c_theta/term );

        // Exposed Combustion Area (Eq 3-2)
        A = .5*PI*bore*( bore + stroke*term2 );

        //--------------------------------------------------------------
        // Weibe Function
        //--------------------------------------------------------------

        thetaSinceSpark = theta - sparkAngle;

        if ( thetaSinceSpark > 0 && thetaSinceSpark < burnEnd ) {
    
            // Weibe Function for mass fraction burn (Eq 3-4)
            // weibe = 1.0 - exp( -5.0*pow( thetaSinceSpark/burnDuration, 3 ) )
            dWeibe_dtheta = - exp( -5.0*pow( thetaSinceSpark*r_burnDuration, 3.0) )*
                (-15.0*pow( thetaSinceSpark*r_burnDuration, 2.0))*r_burnDuration;
    
            //--------------------------------------------------------------
            // Calculate Total Heat Input
            //--------------------------------------------------------------

            // Total Heat Input. (Eq 3-7)
            // Mass_in is integrated as we go from IVO to IVC 
            // .95 because not all mass is burned.
            Q = Qfac*mass_in;

            //--------------------------------------------------------------
            // Calculate Heat Release
            //--------------------------------------------------------------

            // Heat Release. (Eq 3-5)
            dQ_dtheta = Q*dWeibe_dtheta;
        }
        else
            dQ_dtheta = 0.0;

        //--------------------------------------------------------------
        // Cylinder Pressure Model
        //--------------------------------------------------------------

        // Cylinder Pressure. (Eq 3-3)
        P += (((k-1)*(dQ_dtheta - Qloss) - k*P*dV_dtheta)/V)*thetastep;

        // Calculate mass flow only when intake valve is open
        if ( theta <= intake_close && theta >= intake_open ) {

            //--------------------------------------------------------------
            // Valve Lift, Area, and Discharge Coefficient
            //--------------------------------------------------------------

            phi = valve1*( IVO - IVC + 540 + 2.0*thetad );

            // Valve Lift Function. (Eq 3-16)
            Lv = .5*Liv*(1+cos(phi));

            // Valve curtain area. (Eq 3-12)
            Ar = PI*Div*Lv;

            LD = Lv/Div;

            // Discharge coefficient for inlet poppet valve. (Eq 3-18)
            CD = ( 190.47*LD*LD*LD*LD - 143.13*LD*LD*LD +
                   31.248*LD*LD - 2.5999*LD + 0.6913 );
            
            //--------------------------------------------------------------
            // Find pressure ratio for intake flow
            //--------------------------------------------------------------

            // Note 5.5 is a fudge factor that still needs investigation.
            Pratio = (P+5.5*Pmix)/P0;

            // Pratio>1 means outflow
            if ( Pratio>1 ) {
                Pratio = 1.0/Pratio;
                flow_direction = -1.0;
            }
            else
                flow_direction = 1.0;

            if ( Pratio < Pratio_crit )
                Pratio = Pratio_crit;
        
            //--------------------------------------------------------------
            // Calculate Intake Mass Flow
            //--------------------------------------------------------------

            // Mass flow rate. (Eq 3-15)
            // Note, 3-15 is wrong, or an approximation or something
            // Changed to standard orifice equation for better results
            dm_dtheta = flow_direction*CD*Ar*C2*pow( C1*
                    ( pow(Pratio,e1) - pow(Pratio,e2) ), .5 );
            mass_in += dm_dtheta;
            
        }

        //--------------------------------------------------------------
        // Fuel Stochiometry
        //--------------------------------------------------------------

        // Temperature
        Tg = P*V*Mw/((mass_in+m_res)*Ru);

        // Mixture Pressure (kPa)
        Pmix = mass_in*T0*Ru/(Mw*V);

        //--------------------------------------------------------------
        // Calculate Heat Transfer Coefficient
        //--------------------------------------------------------------

        // Hohenberg Correlation. (Eq 3-10)
        h = h_ind * pow(P,0.8) * pow(Tg,-0.4);

        //--------------------------------------------------------------
        // Calculate Heat Loss
        //--------------------------------------------------------------

        // Heat loss to the Cylinder Wall (Kj/radian). (Eq 3-8)
        Qloss = .001*h*A*(Tg-Tw)/t_to_theta;

        //--------------------------------------------------------------
        // Work & Power
        //--------------------------------------------------------------

        // IMEP (Eq 3-23)
        pmi += (P+Pmix)*dV_dtheta;

    }

    // Effective Pressure (Eq 3-24)
    BMEP = pmi*thetastep/disp - FMEP;

    // Effective Power (kwatt=kN*m/sec) (Eq 3-25)
    Power[0] = 0.5*BMEP*RPM*disp*nCyl/60.0;

    // Torque (kN*m->Nm) (Eq 3-26)
    Torque[0] = 30.0*Power[0]/(PI*RPM)*1000.0;

    // Fuel Burn (liters/sec)
    FuelBurn[0] = (nCyl*mass_in*1000.0*RPM)/(60.0*Fuel_Density*(1.0+AFR)*2.0);

    // Engine Wieght (Empirical) (kg)
    EngineWeight[0] = (100.0/.002)*(disp-.001) + 75.0;
    
    
    //return(0);
}
