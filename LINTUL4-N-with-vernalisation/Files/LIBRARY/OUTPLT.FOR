* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE OUTPLT (ITASK, RN)
      IMPLICIT NONE
      INTEGER ITASK
      CHARACTER*(*) RN
      INTEGER IMNNAM
      PARAMETER (IMNNAM=25)
      REAL AVMIN(IMNNAM), AVMAX(IMNNAM)
      CHARACTER*36 AVN(IMNNAM),LXN,LN
      CHARACTER*1 MARK(IMNNAM)
      LOGICAL FOUND(IMNNAM)
      INTEGER IN, ILU1, ILU2, I, I1, I2, I3, I4, I5, I6, IY, ILTASK
      INTEGER IRUN, IWRITE, IFINDC, ILEN, IOS, IEND, ISREC, IEREC, IREC
      REAL LV, LVO, VMIN, VMAX, WIDTH
      CHARACTER*109 RECORD, REC1, REC2
      CHARACTER LXV*13, CHR*1, RUNTYP*1,RUNDUM*1
      LOGICAL FNDONE, OPEND, YFND
      SAVE
      DATA IN /0/, ILU2 /0/
      DATA MARK /'1','2','3','4','5','6','7','8','9','0',
     &'A','B','C','D','E','F','G','H','I','J',
     &'K','L','M','N','P'/
      IF (ITASK.EQ.1) THEN
      LXN = RN
      CALL UPPERC (LXN)
      I4 = IFINDC (AVN, IMNNAM, 1, IN, LXN)
      IF (I4.EQ.0) THEN
      IN = IN+1
      IF (IN.GT.IMNNAM)
     &CALL FATALERR ('OUTPLT',
     &'Too many variables to be plotted')
      AVN(IN) = LXN
      END IF
      ELSE IF ((ITASK.GE.4.AND.ITASK.LE.7).OR.
     &(ITASK.GE.14.AND.ITASK.LE.17)) THEN
      IF (IN.EQ.0) THEN
      WRITE (*,'(A)')
     &' WARNING from OUTPLT: Initialization not done'
      RETURN
      END IF
      ILTASK = MOD (ITASK,10)
      CALL AMBUSY (2,'OUTDAT',ILU2)
      IF (ILU2.EQ.0) THEN
      ILU2 = 20
      ILU1 = ILU2+1
      CALL FOPENG (ILU1,'RES.BIN','OLD','UD',45,' ')
      INQUIRE (UNIT=ILU2, OPENED=OPEND)
      IF (.NOT.OPEND) CALL FOPENG
     &(ILU2,'RES.DAT','NEW','SF',0,'DEL')
      END IF
      ILU1 = ILU2+1
      READ (ILU1,REC=1) RUNTYP,IRUN
      IF (RUNTYP.EQ.'-') THEN
      IEREC = 1
      DO 10 I1=1,IRUN-1
      ISREC = IEREC+1
      READ (ILU1,REC=ISREC) RUNDUM,IEREC
10    CONTINUE
      IOS = 0
      IREC = IEREC+2
20    IF (IOS.EQ.0) THEN
      READ (ILU1,REC=IREC,IOSTAT=IOS) RUNDUM
      IF (IOS.EQ.0) IREC = IREC+1
      GOTO 20
      END IF
      WRITE (ILU1, REC=IEREC+1) ' ',IREC-1,'...........',0.
      WRITE (ILU1, REC=1)       '+',IRUN  ,'...........',0.
      END IF
      IF (ITASK.GE.14) THEN
      I6 = 1
      ELSE
      I6 = IRUN
      END IF
      DO 30 I5=I6,IRUN
      IEREC = 1
      DO 40 I1=1,I5
      ISREC = IEREC+1
      READ (ILU1,REC=ISREC) RUNDUM,IEREC
40    CONTINUE
      READ (ILU1,REC=ISREC+1) RUNTYP, IRUN, LXN
      VMIN = 0.
      VMAX = 0.
      DO 50 I1=1,IMNNAM
      AVMIN(I1) = 0.
      AVMAX(I1) = 0.
      FOUND(I1) = .FALSE.
50    CONTINUE
      FNDONE = .FALSE.
      DO 60 IREC=ISREC+1,IEREC
      READ (ILU1,REC=IREC) RUNDUM, I1, LN, LV
      I = IFINDC (AVN, IMNNAM, 1, IN, LN)
      IF (I.GT.0) THEN
      IF (.NOT.FOUND(I)) THEN
      AVMIN(I) = LV
      AVMAX(I) = LV
      FOUND(I) = .TRUE.
      END IF
      IF (.NOT.FNDONE) THEN
      VMIN = LV
      VMAX = LV
      FNDONE = .TRUE.
      END IF
      IF (LV.LT.AVMIN(I)) AVMIN(I) = LV
      IF (LV.GT.AVMAX(I)) AVMAX(I) = LV
      IF (LV.LT.VMIN) VMIN = LV
      IF (LV.GT.VMAX) VMAX = LV
      END IF
60    CONTINUE
      REC1 = ' '
      REC2 = ' '
      IF (ILTASK.EQ.4 .OR. ILTASK.EQ.5) THEN
      DO 70 I2=2,108
      REC1(I2:I2) = '-'
70    CONTINUE
      REC1(1:1) = 'I'
      REC1(109:109) = 'I'
      DO 80 I2=1,109,18
      REC2(I2:I2) = 'I'
80    CONTINUE
      IEND = 109
      WIDTH = 108.
      ELSE
      DO 90 I2=2,64
      REC1(I2:I2) = '-'
90    CONTINUE
      REC1(1:1) = 'I'
      REC1(65:65) = 'I'
      DO 100 I2=1,65,16
      REC2(I2:I2) = 'I'
100   CONTINUE
      IEND  = 65
      WIDTH = 64.
      END IF
      WRITE (ILU2,'(/,14X,A)') RN
      IF (IRUN.EQ.0) THEN
      WRITE (ILU2,'(14X,A)')
     &'Output plot number   :  0 (=first output plot)'
      ELSE IF (RUNTYP.EQ.'R') THEN
      WRITE (ILU2,'(14X,A,I4)')
     &'Output from rerun set:',IRUN
      ELSE IF (RUNTYP.EQ.'N') THEN
      WRITE (ILU2,'(14X,A,I4)')
     &'Output plot number   :',IRUN
      ELSE
      CALL FATALERR ('OUTPLT','unknown run type')
      END IF
      WRITE (ILU2,'(/,14X,4A,/,14X,4A)')
     &'Variable  ','Marker  ','Minimum value  ','Maximum value',
     &'--------  ','------  ','-------------  ','-------------'
      DO 110 I=1,IN
      IF (FOUND(I)) THEN
      IF (AVMIN(I).NE.AVMAX(I)) THEN
      IF (ILEN (AVN(I)).LE.11) THEN
      WRITE (ILU2,'(14X,A11,2X,A,6X,G11.4,4X,G11.4)')
     &AVN(I),MARK(I),AVMIN(I),AVMAX(I)
      ELSE
      WRITE (ILU2,'(14X,A,/,27X,A,6X,G11.4,4X,G11.4)')
     &AVN(I),MARK(I),AVMIN(I),AVMAX(I)
      END IF
      ELSE
      IF (ILEN (AVN(I)).LE.11) THEN
      WRITE (ILU2,'(14X,A11,2X,A,6X,G11.4,4X,G11.4,A)')
     &AVN(I),MARK(I),AVMIN(I),AVMAX(I),
     &' no range to plot'
      ELSE
      WRITE (ILU2,'(14X,A,/,27X,A,6X,G11.4,4X,G11.4,A)')
     &AVN(I),MARK(I),AVMIN(I),AVMAX(I),
     &' no range to plot'
      END IF
      END IF
      ELSE
      WRITE (*,'(4A)') ' WARNING from OUTPLT: ',
     &'variable ', AVN(I), 'not in set'
      WRITE (ILU2,'(4A)') ' WARNING from OUTPLT: ',
     &'variable ', AVN(I), 'not in set'
      END IF
110   CONTINUE
      IF (ILTASK.EQ.5 .OR. ILTASK.EQ.7) THEN
      WRITE (ILU2,'(/,14X,A,5X,G11.4,4X,G11.4)')
     &'Scaling: Common', VMIN, VMAX
      ELSE
      WRITE (ILU2,'(/,14X,A)')
     &'Scaling: Individual'
      END IF
      WRITE (ILU2,'(/,3X,A,/)') LXN
      YFND = .FALSE.
      IWRITE = 0
      RECORD = REC1
      DO 120 I2=1,IMNNAM
      FOUND(I2) = .FALSE.
120   CONTINUE
      DO 130 IREC=ISREC+1,IEREC
      READ (ILU1,REC=IREC) RUNDUM, I2, LN, LV
      IF (LN.EQ.LXN) THEN
      IF (YFND) THEN
      WRITE (LXV,'(1P,G13.6)') LVO
      WRITE (ILU2,'(2(1X,A))') LXV, RECORD(1:IEND)
      IWRITE = IWRITE+1
      IF (MOD (IWRITE,10).EQ.0) THEN
      RECORD = REC1
      ELSE
      RECORD = REC2
      END IF
      YFND = .FALSE.
      DO 140 I2=1,IMNNAM
      FOUND(I2) = .FALSE.
140   CONTINUE
      END IF
      LVO = LV
      ELSE
      I = IFINDC (AVN, IMNNAM, 1, IN, LN)
      IF (I.GT.0) THEN
      IF (.NOT.FOUND(I)) THEN
      YFND = .TRUE.
      FOUND(I) = .TRUE.
      IF (ILTASK.EQ.4 .OR. ILTASK.EQ.6) THEN
      VMAX = AVMAX(I)
      VMIN = AVMIN(I)
      END IF
      IF (VMAX.NE.VMIN) THEN
      IY = 1+NINT(WIDTH*(LV-VMIN)/(VMAX-VMIN))
      CHR = RECORD(IY:IY)
      IF (CHR.EQ.' '.OR.
     &CHR.EQ.'I'.OR.
     &CHR.EQ.'-') THEN
      RECORD(IY:IY) = MARK(I)
      ELSE
      RECORD(IY:IY) = '*'
      END IF
      END IF
      ELSE
      I2 = ILEN (LN)
      I3 = ILEN (LXN)
      WRITE (*,'(4A)')
     &' WARNING from OUTPLT: variable ',LN(1:I2),
     &' occurs twice at same value of ',LXN(1:I3)
      WRITE (ILU2,'(4A)')
     &' WARNING from OUTPLT: variable ',LN(1:I2),
     &' occurs twice at same value of ',LXN(1:I3)
      END IF
      END IF
      END IF
130   CONTINUE
      WRITE (LXV,'(1P,G13.6)') LVO
      IF (YFND) WRITE (ILU2,'(2(1X,A))') LXV, RECORD(1:IEND)
      WRITE (ILU2,'(/,/,/,A)') ' '
30    CONTINUE
      IN = 0
      ELSE
      CALL FATALERR ('OUTPLT','Wrong ITASK')
      END IF
      RETURN
      END
