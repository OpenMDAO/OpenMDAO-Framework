//
//------------------------------------------------------------------------
//                                                                       |
//   File Name:     eng_state_tbl.fnc                                    |
//   Date(s):       February 1, 2007                                     |
//   Author:        Scott M. Jones                                       |
//                                                                       |
//   Description:   File to Create Engine State Tables for ANOPP         |
//                                                                       |
//------------------------------------------------------------------------


// function to save noise data:  m = throttle #,  n = mach/alt #,  var#
//    the six dimensionless variables for each component are:
//    Area/A_ref, fuel-to-air ratio, massflow rate/M_ref, Pt/P_ref, 
//    Tt/T_ref, and shaft speed/SPDref

void save_noise_data(string Ambient,
                     string Inlet,
                     string Fan,
                     string LPT,
                     string LP_Shaft,
                     string Burner,
                     string Core_Nozz,
                     string Byp_Nozz,
                     string PERF) { 

   M_ref = A_ref * Ambient->Ps * C_GRAVITY / 
          sqrt( Inlet->Fl_I.Rs * Ambient->Ts * C_BTUtoFT_LBF * C_GRAVITY );

   // FAN 1 VARS
   noise_var[m][n][0] = 0.0;
   noise_var[m][n][1] = 0.0;
   noise_var[m][n][2] = Fan->Fl_I.W  / M_ref;
   noise_var[m][n][3] = 0.0;
   noise_var[m][n][4] = Fan->Fl_I.Tt / Ambient->Ts;
   noise_var[m][n][5] = (LP_Shaft->Nmech*LP_RPM_scalar)*
       (WATE_Fan.stg1TipRadius*2.)/
       sqrt(Inlet->Fl_I.Rs*Ambient->Ts * C_BTUtoFT_LBF * C_GRAVITY)/60.;

   // FAN 2 VARS
   noise_var[m][n][6] = 0.0;
   noise_var[m][n][7] = 0.0;
   noise_var[m][n][8] = 0.0;
   noise_var[m][n][9] = 0.0;
   noise_var[m][n][10] = Fan->Fl_O.Tt / Ambient->Ts;
   noise_var[m][n][11] = 0.0;

   // CORE 1 VARS
   noise_var[m][n][12] = 0.0;
   noise_var[m][n][13] = 0.0;
   noise_var[m][n][14] = Burner->Fl_I.W  / M_ref;
   noise_var[m][n][15] = Burner->Fl_I.Pt / Ambient->Ps;
   noise_var[m][n][16] = Burner->Fl_I.Tt / Ambient->Ts;
   noise_var[m][n][17] = 0.0;

   // CORE 2 VARS
   noise_var[m][n][18] = 0.0;
   noise_var[m][n][19] = 0.0;
   noise_var[m][n][20] = 0.0;
   noise_var[m][n][21] = 0.0;
   noise_var[m][n][22] = Burner->Fl_O.Tt / Ambient->Ts;
   noise_var[m][n][23] = 0.0;

   // TURBINE 1 VARS
   noise_var[m][n][24] = 0.0;
   noise_var[m][n][25] = 0.0;
   noise_var[m][n][26] = Core_Nozz->Fl_I.W / M_ref;
   noise_var[m][n][27] = 0.0;
   noise_var[m][n][28] = Burner->Fl_O.Tt / Ambient->Ts;
   noise_var[m][n][29] = (LP_Shaft->Nmech*LP_RPM_scalar)*
       (WATE_LPT.tipRadius_stg[last_LPT_stg]*2.0/12.0)/
        sqrt(Inlet->Fl_I.Rs*Ambient->Ts * C_BTUtoFT_LBF * C_GRAVITY)/60.;

   // TURBINE 2 VARS
   noise_var[m][n][30] = WATE_LPT.area_stg[last_LPT_stg]*144./A_ref;
   noise_var[m][n][31] = Core_Nozz->Fl_I.FAR;
   noise_var[m][n][32] = Core_Nozz->Fl_I.W  / M_ref;
   noise_var[m][n][33] = Core_Nozz->Fl_I.Pt / Ambient->Ps;
   noise_var[m][n][34] = Core_Nozz->Fl_I.Tt / Ambient->Ts;
   noise_var[m][n][35] = 0.0;

   // JET 1 VARS
   noise_var[m][n][36] = Core_Nozz->Fl_O.Aphy / A_ref;
   noise_var[m][n][37] = Burner->FAR;
   noise_var[m][n][38] = Core_Nozz->Fl_I.W  / M_ref;
   noise_var[m][n][39] = Core_Nozz->Fl_I.Pt / Ambient->Ps;
   noise_var[m][n][40] = Core_Nozz->Fl_I.Tt / Ambient->Ts;
   noise_var[m][n][41] = 0.0;

   // JET 2 VARS
   noise_var[m][n][42] = Byp_Nozz->Fl_O.Aphy / A_ref;
   noise_var[m][n][43] = 0.0;
   noise_var[m][n][44] = Byp_Nozz->Fl_I.W  / M_ref;
   noise_var[m][n][45] = Byp_Nozz->Fl_I.Pt / Ambient->Ps;
   noise_var[m][n][46] = Byp_Nozz->Fl_I.Tt / Ambient->Ts;
   noise_var[m][n][47] = 0.0;
   
   // MISC. VARS
   noise_var[m][n][48] = Ambient->MN;
   noise_var[m][n][49] = Ambient->alt;
   noise_var[m][n][50] = PERF->Fn/MaxThrust;

   ++m;
   if (m == number_of_throttles) { m=0; ++n; }

   // update summary Dataviewer
   NviewSumm.update(); 
} 


OutFileStream EST_Stream { filename = "ANOPP.EST"; } 
void write_eng_state_tbl() { 
   real outvalue;
   int i,j,k;

   string FAN1, FAN2, CORE1, CORE2, TURB1, TURB2, JET1, JET2;
   string SINT, SIND1, SIND2, SIND3, SDEP; 

   SINT  = "  INT = 0 1 \n";
 //SIND1 = "  IND1 = RS 7 0 0 1.00 0.90 0.80 0.60 0.40 0.20 0.07\n";
 //SIND2 = "  IND2 = RS 7 0 0 0.00 0.10 0.20 0.30 0.40 0.50 0.60\n"; 
   SIND3 = "  IND3 = 0 6 0 0\n";
   SDEP  = "  DEP = RS\n";
   for ( i=0; i<number_of_throttles; ++i ) {
      SIND1 = SIND1 + toStr( noise_var[i][0][50], "f5.2" );
   }
   SIND1 = "  IND1 = RS " + toStr(number_of_throttles) + " 0 0" + SIND1 + "\n";

   for ( i=0; i<number_of_machs; ++i ) {
      SIND2 = SIND2 + toStr( noise_var[0][i][48], "f5.2" );
   }
   SIND2 = "  IND2 = RS " + toStr(number_of_machs) + " 0 0" + SIND2 + "\n";

FAN1 = 
" $ \n" +
" $----------------------------------------------------------------------\n" +
" $ ENGINE STATE TABLES FOR THE FAN \n" + 
"  TABLE ENG(FAN1) 1 SOURCE=* $\n" + SINT + SIND1 + SIND2 + SIND3 + SDEP;

FAN2 = 
"  TABLE ENG(FAN2) 1 SOURCE=* $\n" + SINT + SIND1 + SIND2 + SIND3 + SDEP;

CORE1 = 
" $\n" + 
" $----------------------------------------------------------------------\n" + 
" $ ENGINE STATE TABLES FOR THE CORE\n" + 
"  TABLE ENG(CORE1) 1 SOURCE=* $\n" + SINT + SIND1 + SIND2 + SIND3 + SDEP;

CORE2 = 
"  TABLE ENG(CORE2) 1 SOURCE=* $\n" + SINT + SIND1 + SIND2 + SIND3 + SDEP;

TURB1 = 
" $\n" + 
" $----------------------------------------------------------------------\n" + 
" $ ENGINE STATE TABLES FOR THE TURBINE\n" + 
"  TABLE ENG(TURBINE1) 1 SOURCE=* $\n" + SINT + SIND1 + SIND2 + SIND3 + SDEP;

TURB2 = 
"  TABLE ENG(TURBINE2) 1 SOURCE=* $\n" + SINT + SIND1 + SIND2 + SIND3 + SDEP;

JET1 = 
" $\n" + 
" $----------------------------------------------------------------------\n" + 
" $ ENGINE STATE TABLES FOR THE JET\n" + 
"  TABLE ENG(PRIM) 1 SOURCE=* $\n" + SINT + SIND1 + SIND2 + SIND3 + SDEP;

JET2 = 
"  TABLE ENG(SEC) 1 SOURCE=* $\n" + SINT + SIND1 + SIND2 + SIND3 + SDEP;


   for ( i=0; i<number_of_vars-3; ++i ) { 
   
      if ( i ==  0 ) { EST_Stream << FAN1;  }
      if ( i ==  6 ) { EST_Stream << FAN2;  }
      if ( i == 12 ) { EST_Stream << CORE1; }
      if ( i == 18 ) { EST_Stream << CORE2; }
      if ( i == 24 ) { EST_Stream << TURB1; }
      if ( i == 30 ) { EST_Stream << TURB2; }
      if ( i == 36 ) { EST_Stream << JET1;  }
      if ( i == 42 ) { EST_Stream << JET2;  }

      for ( j=0; j<number_of_machs; ++j ) { 
         for ( k=0; k<number_of_throttles; ++k ) {
            outvalue = noise_var[k][j][i];
            if ( outvalue <   10. ) { EST_Stream << toStr( outvalue, "f8.4"); } 
       else if ( outvalue <  100. ) { EST_Stream << toStr( outvalue, "f8.3"); } 
       else if ( outvalue < 1000. ) { EST_Stream << toStr( outvalue, "f8.2"); } 
       else if ( outvalue >=1000. ) { EST_Stream << toStr( outvalue, "f8.1"); } 
         } 
         EST_Stream << "\n";
      } 

      if ( i ==  5 ) { EST_Stream << "  END* $\n"; }
      if ( i == 11 ) { EST_Stream << "  END* $\n"; }
      if ( i == 17 ) { EST_Stream << "  END* $\n"; }
      if ( i == 23 ) { EST_Stream << "  END* $\n"; }
      if ( i == 29 ) { EST_Stream << "  END* $\n"; }
      if ( i == 35 ) { EST_Stream << "  END* $\n"; }
      if ( i == 41 ) { EST_Stream << "  END* $\n"; }
      if ( i == 47 ) { EST_Stream << "  END* $\n"; }
   } 
}

