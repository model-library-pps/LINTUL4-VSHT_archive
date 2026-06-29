** $Id: dtardp.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE DTARDP (DATEA,FSEC,DPDTTM)
      IMPLICIT NONE
      INTEGER DATEA(6)
      REAL FSEC
      DOUBLE PRECISION DPDTTM
      INTEGER ITASK
      LOGICAL ERR
      CHARACTER*80 MESSAG
      SAVE
      ITASK = 1
      CALL DTSYS (ITASK,DATEA,FSEC,DPDTTM,ERR,MESSAG)
      IF (ERR) THEN
      DPDTTM = 0.D0
      CALL FATALERR ('DTARDP',MESSAG)
      END IF
      RETURN
      END
