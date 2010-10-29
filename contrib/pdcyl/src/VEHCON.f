      SUBROUTINE VEHCON (NTB,K)

C ACRONYM
C     VEHicle CONcept
C     ***     ***
C
C PURPOSE
C     Find various structural parameters for a given vehicle concept.
C
C ARGUMENTS
C     TYPE     VARIABLE   I/O  DESCRIPTION                      COMMON        
C.......................................................................
C
      INTEGER  NTB
C                          I   Top or Bottom code
      INTEGER  K
C                          I   break point location index
C
C
C ENVIRONMENT
C     Machine:               IRIS Indigo
C     Operating System:      UNIX 
C     Language:              FORTRAN 77       
C
C AUTHOR
C     Mark D. Ardema
C     Ames Research Center/Santa Clara University
C
C CHANGE HISTORY
C     DATE         DESCRIPTION 
C....................................................................... 
C     ??? ??  mda  Original release - MOD 0.
C     Sep 93  cdr  Added standard comments, changed method of 
C                  assigning parameters to eliminate go to loops.
C                  sent index values through argument rather than 
C                  common blocks.
C     JULY 94 MCC  REMOVED ALL HEAT TRANSFER CALCULATIONS PRIOR TO
C                  INTEGRATION INTO ACSYNT
C-*******1*********2*********3*********4*********5*********6*********7**
C
C COMMON PARAMETER VALUES
C     TYPE         VARIABLE            DESCRIPTION         
C.......................................................................

C  INCLUDE STATEMENTS FOR PDCYLA.FOR ON VAX (RUNNING IN SAME DIRECTORY)

      INCLUDE 'VEHICON.INC'

C  USE EXISTING COMMON BLOCK FOR INSULATION
      COMMON /CONCEPT/ BRAT,TRAT,ANGLR,MARK,KX
      REAL KX

C LOCAL DECLARATIONS
C     TYPE         VARIABLE       VAL  DESCRIPTION         
C.......................................................................
      REAL         ARICON(12),AREFF(12),ARSE(12),ARCK(12),ARCPR(12)
      REAL         ARCTH(12),ARBRAT(12),ARTRAT(12),ARANG(12),ARKX(12)
      REAL         ARMARK(12)
C
      DATA         ARICON 
     1               /      1,      1,      1,      1,      1,      1,
     2                      2,      2,      2,      2,      1,      0 /
C                                      ARray of ICON values
      DATA         AREFF  
     1               / 0.8230, 0.6560, 0.9110, 0.7600, 0.7600, 0.6050,
     2                 7.2600, 0.4423, 0.3615, 0.3615, 1.6000, 0.0000 /
C                                      ARray of EFF values
      DATA         ARSE  
     1               / 3.0000, 2.0000, 2.0000, 2.0000, 2.0000, 2.0000,
     2                 2.5400, 1.6670, 1.6670, 1.6670, 2.0000, 0.0000 /
C                                      ARray of SE values
      DATA         ARCK  
     1               / 1.0000, 2.4630, 2.4750, 2.0390, 2.6280, 4.3100,
     2                 1.0000, 4.8200, 3.4130, 2.7700, 2.7700, 0.0000 /
C                                      ARray of CK values
      DATA         ARCPR 
     1               / 1.0000, 2.4630, 2.4750, 1.8350, 1.5760, 3.9650,
     2                 1.0000, 3.1320, 3.4130, 2.7700, 2.7700, 0.0000 /
C                                      ARray of CPR values
      DATA         ARCTH 
     1               / 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     2                 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000 /
C                                      ARray of CTHick values
      DATA         ARBRAT
     1               / 0.0000, 0.6500, 0.8700, 0.5800, 0.6000, 0.0000,
     1                 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000 /
C                                      ARray of BRAT values
      DATA         ARTRAT 
     1               / 0.0000, 2.2500, 1.0600, 0.9000, 0.6000, 0.9200,
     2                 0.0000, 0.6500, 1.0000, 0.0000, 0.0000, 0.0000 /
C                                      ARray of TRAT values
      DATA         ARANG 
     1               / 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 1.0820,
     2                 0.0000, 0.9599, 0.7854, 0.0000, 0.0000, 0.0000 /
C                                      ARray of ANGlr values
      DATA         ARKX   
     1               / 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 4.1500,
     2                 0.0000, 3.5000, 5.3000, 0.0000, 0.0000, 0.0000 /
C                                      ARray of KX values
      DATA         ARMARK 
     1               / 1.0000, 2.0000, 3.0000, 3.0000, 3.0000, 4.0000,
     2                 5.0000, 6.0000, 6.0000, 0.0000, 0.0000, 0.0000 /
C                                      ARray of MARK values
      
      IF (NTB.EQ.1) THEN
C        Top
         ES = EST(K) * KDE
         EF = EFT(K) * KDE
         FTS = FTST(K) * KDF
         FCS = FCST(K) * KDF
         DS = DST(K)
         DF = DFT(K)
         TMG = TMGT(K)
         PG = PGT(K)
         TEMP = TEMPT(K)
         JCON = JCONT(K)
         KCON = KCONT(K)

      ELSE
C        Bottom
         ES = ESB(K)* KDE
         EF = EFB(K)* KDE
         FTS = FTSB(K)* KDF
         FCS = FCSB(K)* KDF
         DS = DSB(K)
         DF = DFB(K)
         TMG = TMGB(K)
         PG = PGB(K)
         TEMP = TEMPB(K)
         JCON = JCONB(K)
         KCON = KCONB(K)
      ENDIF

      IF ( KCON .LE. 11 ) THEN
         ICON = ARICON(KCON)
         EFF = AREFF(KCON)
         SE = ARSE(KCON)
         CK = ARCK(KCON)
         CPR = ARCPR(KCON)
         CTHICK = ARCTH(KCON)
         BRAT = ARBRAT(KCON)
         TRAT = ARTRAT(KCON)
         ANGLR = ARANG(KCON)
         KX = ARKX(KCON)
         MARK = ARMARK(KCON)
      ENDIF

      RETURN
      END
     
