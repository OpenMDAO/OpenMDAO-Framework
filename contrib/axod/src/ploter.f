      SUBROUTINE PLOTER
c
c     WRITES MAP PLOT FILE IN NEPP FORMAT
C
      COMMON /PLOTT/ ENDPLT,IPCAS,IPSCAS(20),xnname(20),iplot(3)
      common /pltdat/ speed(20),prt(250,20),efft(250,20),flow(250,20)
      dimension js(20)
      DATA ITRNE,ITRNW/269,270/                                         PAR21830
C
C     SORT SPEEDS TO ASCENDING ORDER
	open (unit=7, file="fort.7", status='unknown')
C
      nsort=1
      do 3 i=1,ipcas
    3 js(i)=i
      do while (nsort.eq.1)
        nsort=0
        ks=ipcas-1
        do 5 is=1,ks
          if (speed(is).gt.speed(is+1)) then
            temp=speed(is)
            speed(is)=speed(is+1)
            speed(is+1)=temp
            jtemp=js(is)
            js(is)=js(is+1)
            js(is+1)=jtemp
            nsort=1
          end if
    5   continue
      end do
      ifci=7
      ifcj=7
      WRITE (IFCI,170) ITRNE                                            PAR22560
      nanga2=1
      arn=1.0
60    WRITE (IFCI,180) NANGA2,ARN                                       PAR22600
      nspds=ipcas
70    DO 80 JJ=1,NSPDS,7                                                PAR22610
      JEND=JJ+6                                                         PAR22620
      IF (JEND.GT.NSPDS) JEND=NSPDS                                     PAR22630
80    WRITE (IFCI,190) NSPDS,(speed(J),J=JJ,JEND)                       PAR22640
      DO 100 J=1,NSPDS                                                  PAR22690
      ks=js(j)
      npr=ipscas(ks)
      DO 90 KK=1,NPR,7                                                  PAR22650
      KEND=KK+6                                                         PAR22660
      IF (KEND.GT.NPR) KEND=NPR                                         PAR22670
90    WRITE (IFCI,200) NPR,(PRt(K,ks),K=KK,KEND)                           PAR22
      DO 100 KK=1,NPR,7                                                 PAR22700
      KEND=KK+6                                                         PAR22710
      IF (KEND.GT.NPR) KEND=NPR                                         PAR22720
100   WRITE (IFCI,210) NPR,(EFFt(K,ks),K=KK,KEND)                        PAR2273
      WRITE (IFCI,230)                                                  PAR22740
      WRITE (IFCJ,220) ITRNW                                            PAR22760
110   WRITE (IFCJ,180) NANGA2,ARN                                       PAR22800
120   DO 130 JJ=1,NSPDS,7                                               PAR22810
      JEND=JJ+6                                                         PAR22820
      IF (JEND.GT.NSPDS) JEND=NSPDS                                     PAR22830
130   WRITE (IFCJ,190) NSPDS,(speed(J),J=JJ,JEND)                       PAR22840
      DO 150 J=1,NSPDS                                                  PAR22890
      ks=js(j)
      npr=ipscas(ks)
      DO 140 KK=1,NPR,7                                                 PAR22850
      KEND=KK+6                                                         PAR22860
      IF (KEND.GT.NPR) KEND=NPR                                         PAR22870
140   WRITE (IFCJ,200) NPR,(PRt(K,ks),K=KK,KEND)                           PAR22
      DO 150 KK=1,NPR,7                                                 PAR22900
      KEND=KK+6                                                         PAR22910
      IF (KEND.GT.NPR) KEND=NPR                                         PAR22920
150   WRITE (IFCJ,240) NPR,(flow(K,ks),K=KK,KEND)                        PAR2293
      WRITE (IFCJ,230)
	close (unit=7)                                                     
C                                                                       PAR22970
170   FORMAT (1X,I4,7X,'TURBINE EFFICIENCY VS. PR, RPM, AND AREA',23X)  PAR22980
180   FORMAT ('AREA',1X,I2,3X,7F10.2)                                   PAR22990
190   FORMAT ('SPED',1X,I2,3X,7F10.1)                                   PAR23000
200   FORMAT ('PR  ',1X,I2,3X,7F10.3)                                   PAR23010
210   FORMAT ('EFF ',1X,I2,3X,7F10.4)                                   PAR23020
220   FORMAT (1X,I4,7X,'TURBINE FLOW FUNCTION VS. PR, RPM, ','AND AREA',PAR23030
     120X)                                                              PAR23040
230   FORMAT ('EOT')                                                    PAR23050
240   FORMAT ('FLOW',1X,I2,3X,7F10.3)                                   PAR23060
      RETURN
      END
