      SUBROUTINE PLOTER
c
c     WRITES MAP PLOT FILE IN NEPP FORMAT
C
      COMMON /PLOTT/ ENDPLT,IPCAS,IPSCAS(20),xnname(20),iplot(3)
      common /pltdat/ speed(20),prt(250,20),efft(250,20),flow(250,20)
      dimension js(20)
      DATA ITRNE,ITRNW/269,270/
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
      WRITE (IFCI,170) ITRNE
      nanga2=1
      arn=1.0
      WRITE (IFCI,180) NANGA2,ARN
      nspds=ipcas
      DO 80 JJ=1,NSPDS,7
      JEND=JJ+6
      IF (JEND.GT.NSPDS) JEND=NSPDS
80    WRITE (IFCI,190) NSPDS,(speed(J),J=JJ,JEND)
      DO 100 J=1,NSPDS
      ks=js(j)
      npr=ipscas(ks)
      DO 90 KK=1,NPR,7
      KEND=KK+6
      IF (KEND.GT.NPR) KEND=NPR
90    WRITE (IFCI,200) NPR,(PRt(K,ks),K=KK,KEND)
      DO 100 KK=1,NPR,7
      KEND=KK+6
      IF (KEND.GT.NPR) KEND=NPR
100   WRITE (IFCI,210) NPR,(EFFt(K,ks),K=KK,KEND)
      WRITE (IFCI,230)
      WRITE (IFCJ,220) ITRNW
      WRITE (IFCJ,180) NANGA2,ARN
      DO 130 JJ=1,NSPDS,7
      JEND=JJ+6
      IF (JEND.GT.NSPDS) JEND=NSPDS
130   WRITE (IFCJ,190) NSPDS,(speed(J),J=JJ,JEND)
      DO 150 J=1,NSPDS
      ks=js(j)
      npr=ipscas(ks)
      DO 140 KK=1,NPR,7
      KEND=KK+6
      IF (KEND.GT.NPR) KEND=NPR
140   WRITE (IFCJ,200) NPR,(PRt(K,ks),K=KK,KEND)
      DO 150 KK=1,NPR,7
      KEND=KK+6
      IF (KEND.GT.NPR) KEND=NPR
150   WRITE (IFCJ,240) NPR,(flow(K,ks),K=KK,KEND)
      WRITE (IFCJ,230)
      close (unit=7)
C
170   FORMAT (1X,I4,7X,'TURBINE EFFICIENCY VS. PR, RPM, AND AREA',23X)
180   FORMAT ('AREA',1X,I2,3X,7F10.2)
190   FORMAT ('SPED',1X,I2,3X,7F10.1)
200   FORMAT ('PR  ',1X,I2,3X,7F10.3)
210   FORMAT ('EFF ',1X,I2,3X,7F10.4)
220   FORMAT (1X,I4,7X,'TURBINE FLOW FUNCTION VS. PR, RPM, ','AND AREA',
     120X)
230   FORMAT ('EOT')
240   FORMAT ('FLOW',1X,I2,3X,7F10.3)
      RETURN
      END
