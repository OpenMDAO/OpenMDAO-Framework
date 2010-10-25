       SUBROUTINE GEOSHELL (ICOMND,MARK,TB,ANGLR,TRAT,BRAT,KX,S,FS,ES,
     1                      TS,TW,BW,BS,BF,HIGH,AF)
C
C ACRONYM
C     GEOmetry of SHELL structures
C     ***         *****
C
C PURPOSE
C     Specific geometry calculations of shell structures.
C
C AUTHOR
C     Fiona Saunders
C     Santa Clara University
C
C CHANGE HISTORY
C     DATE         DESCRIPTION 
C....................................................................... 
C     Jul 93  fs   Original release - MOD 0.
C     Nov 93  cdr  Moved Fiona's shell structure geometry to this 
C                  subroutine.

C     INPUT VARIABLES
      INTEGER      ICOMND,MARK
      REAL         TB,ANGLR,TRAT,BRAT,KX,S,FS,ES,TS
C     OUTPUT VARIABLES
      REAL         TW,BW,BS,BF,HIGH,AF


      IF (ICOMND .EQ. 2) THEN
C     Set geometry of shell structure 

            IF (MARK .EQ. 1) THEN
C           Flat plate shell
                  TS = TB
                  TFF = 0.
                  TW = 0.
                  BW = 0.
                  BS = 0.
                  BF = 0.
                  ANGL = ANGLR*57.296

            ELSEIF (MARK.EQ.2) THEN
C           Simple stringer shell
                  TS = TB/(1.+TRAT*BRAT)
                  TFF = 0.
                  TW = TS*TRAT
                  BS = FS*1.1*(1.+TRAT*BRAT)*((S/FS/1./ES)/(TB/FS)/
     1                (TRAT*BRAT**3)/(4.+TRAT*BRAT))**(0.5)
                  BW = BRAT*BS
                  BF = 0.
                  ANGL = ANGLR*57.296

            ELSEIF (MARK.EQ.3) THEN
C           Z-stiffened shell
                  TS = TB/(1.+1.6*BRAT*TRAT)
                  TW = TS*TRAT
                  TFF = TW
                  BW = FS*0.4*(1.+1.6*BRAT*TRAT)/(BRAT*TRAT*(1.+0.59*
     1                 BRAT*TRAT))**(0.5)*(S/ES/TB/1.)**(0.5)
                  BS = BW/BRAT
                  BF = 0.3*BW
                  ANGLR = ANGLR*57.296

            ELSEIF (MARK .EQ. 4) THEN
C           Truss core shell with frames
                 TS = TB/(2.+TRAT/COS(ANGLR))
                 TW = TS*TRAT
                 TFF = 0.
                 BF = 0.
                 ANGLR = ANGLR*57.296
                 BS = 0.95*TS*(KX*(TB/FS)/(S/FS/1./ES))**(0.5)
                 BW = BS/2*TAN(ANGLR)

            ELSEIF (MARK.EQ.5) THEN
C           Monocoque cylinder
                TS = TB
                TFF = 0.
                TW = 0.
                BW = 0.
                BS = 0.
                BF = 0.
                ANGLR = ANGLR*57.296

            ELSEIF (MARK.EQ.6) THEN
C           Truss core shell without frames
                TS = TB/(2.+TRAT/COS(ANGLR))
                TW = TS*TRAT
                ANGLR = ANGLR*57.296
                BS = 0.95*TS*(KX*(TB/RADIUS)/(S/RADIUS/1./ES))**(0.5)
                BW = BS/2*TAN(ANGLR)
                BF = 0.
                TFF = 0.
            ENDIF

C           Calculate flange height for frames
            HIGH = 5.19*(AF)**0.5
      ENDIF

      RETURN

      END
