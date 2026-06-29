** $Id: dtdpar.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE DTDPAR (DPDTTM,DATEA,FSEC)
      IMPLICIT NONE
      INTEGER DATEA(6)
      REAL FSEC
      DOUBLE PRECISION DPDTTM
      INTEGER ITASK,I1
      LOGICAL ERR
      CHARACTER*80 MESSAG
      SAVE
      ITASK = 2
      CALL DTSYS (ITASK,DATEA,FSEC,DPDTTM,ERR,MESSAG)
      IF (ERR) THEN
      DO 10 I1=1,6
      DATEA(I1) = 0
10    CONTINUE
      FSEC = 0.
      CALL FATALERR ('DTDPAR',MESSAG)
      END IF
      RETURN
      END
