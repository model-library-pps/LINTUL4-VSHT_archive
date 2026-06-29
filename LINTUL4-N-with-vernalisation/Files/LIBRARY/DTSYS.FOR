** $Id: dtsys.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE DTSYS (ITASK,DATEA,FSEC,DPDTTM,ERR,MESSAG)
      IMPLICIT NONE
      INTEGER ITASK, DATEA(6)
      LOGICAL ERR
      REAL FSEC
      DOUBLE PRECISION DPDTTM
      CHARACTER*(*) MESSAG
      INTEGER I1, NZERO, DAYMAX, NDAYS
      INTEGER IMOTB1(12),IMOTB2(12)
      DOUBLE PRECISION DTMP
      LOGICAL DTLEAP
      SAVE
      DATA IMOTB1 /0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IMOTB2 /31,28,31,30,31,30,31,31,30,31,30,31/
      ERR    = .FALSE.
      MESSAG = ' '
      IF (ITASK.EQ.1) THEN
      DTMP   = 0.D0
      NZERO  = 0
      DO 10 I1=1,3
      IF (DATEA(I1).EQ.0) NZERO = NZERO+1
10    CONTINUE
      IF (NZERO.GT.0.AND.NZERO.LT.3) THEN
      ERR    = .TRUE.
      MESSAG = 'date not allowed'
      RETURN
      END IF
      IF (DATEA(1).NE.0) THEN
      IF (DATEA(1).GE.1900.AND.DATEA(1).LE.2000) THEN
      DO 20 I1=1900,DATEA(1)-1
      DTMP = DTMP+365.D0
      IF (DTLEAP(I1)) DTMP = DTMP+1.D0
20    CONTINUE
      ELSE
      ERR    = .TRUE.
      MESSAG = 'illegal year'
      RETURN
      END IF
      END IF
      IF (DATEA(2).NE.0) THEN
      IF (DATEA(2).GE.1.AND.DATEA(2).LE.12) THEN
      IF (DATEA(2).GE.2) THEN
      DTMP = DTMP+IMOTB1(DATEA(2))
      IF (DTLEAP(DATEA(1)).AND.DATEA(2).GT.2)
     &DTMP = DTMP+1.D0
      END IF
      ELSE
      ERR    = .TRUE.
      MESSAG = 'month invalid'
      RETURN
      END IF
      END IF
      IF (DATEA(3).NE.0) THEN
      DAYMAX = IMOTB2(DATEA(2))
      IF (DTLEAP (DATEA(1)).AND.DATEA(2).EQ.2) DAYMAX = DAYMAX+1
      IF (DATEA(3).GE.1.AND.DATEA(3).LE.DAYMAX) THEN
      DTMP = DTMP+DBLE (DATEA(3))
      ELSE
      ERR    = .TRUE.
      MESSAG = 'day invalid'
      RETURN
      END IF
      END IF
      IF (DATEA(4).GE.0) THEN
      DTMP = DTMP+DBLE (DATEA(4))/24.D0
      ELSE
      ERR    = .TRUE.
      MESSAG = 'hour invalid'
      RETURN
      END IF
      IF (DATEA(5).GE.0.AND.DATEA(5).LE.59) THEN
      DTMP = DTMP+DBLE (DATEA(5))/1440.D0
      ELSE
      ERR    = .TRUE.
      MESSAG = 'minute invalid'
      RETURN
      END IF
      IF (DATEA(6).GE.0.AND.DATEA(6).LE.59) THEN
      DTMP = DTMP+DBLE (DATEA(6))/86400.D0
      ELSE
      ERR    = .TRUE.
      MESSAG = 'seconds invalid'
      RETURN
      END IF
      IF (FSEC.GE.0.AND.FSEC.LT.1.) THEN
      DTMP = DTMP+DBLE (FSEC)/86400.D0
      ELSE
      ERR    = .TRUE.
      MESSAG = 'fractional seconds invalid'
      RETURN
      END IF
      DPDTTM = DTMP
      ELSE IF (ITASK.EQ.2) THEN
      IF (DPDTTM.LT.1.D0) THEN
      ERR    = .TRUE.
      MESSAG = 'negative date value not allowed'
      RETURN
      END IF
      DTMP = DPDTTM
      DATEA(1) = 1900
      NDAYS    = 365
      IF (DTLEAP (DATEA(1))) NDAYS = NDAYS+1
110   IF (DTMP.GT.DBLE (NDAYS)) THEN
      DTMP     = DTMP-DBLE(NDAYS)
      DATEA(1) = DATEA(1)+1
      NDAYS    = 365
      IF (DTLEAP (DATEA(1))) NDAYS = NDAYS+1
      GOTO 110
      END IF
      DATEA(2) = 1
      NDAYS    = IMOTB2(DATEA(2))
      IF (DTLEAP (DATEA(1)).AND.DATEA(2).EQ.2) NDAYS = NDAYS+1
120   IF (DTMP.GT.DBLE (NDAYS)) THEN
      DTMP     = DTMP-DBLE (NDAYS)
      DATEA(2) = DATEA(2)+1
      NDAYS    = IMOTB2(DATEA(2))
      IF (DTLEAP (DATEA(1)).AND.DATEA(2).EQ.2) NDAYS = NDAYS+1
      GOTO 120
      END IF
      DATEA(3) = INT (DTMP)
      DTMP     = DTMP-DBLE (DATEA(3))
      DATEA(4) = INT (DTMP*24.D0)
      DTMP     = DTMP-DBLE (DATEA(4))/24.D0
      DATEA(5) = INT (DTMP*1440.D0)
      DTMP     = DTMP-DBLE (DATEA(5))/1440.D0
      DATEA(6) = INT (DTMP*86400.D0)
      DTMP     = DTMP-DBLE (DATEA(6))/86400.D0
      FSEC = REAL (DTMP*86400.D0)
      ELSE
      CALL FATALERR ('DTSYS','unknown task')
      END IF
      RETURN
      END
