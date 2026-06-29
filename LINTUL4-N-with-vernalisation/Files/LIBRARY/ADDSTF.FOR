** $Id: addstf.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE ADDSTF (STRING,SIGLEN,TMP)
      IMPLICIT NONE
      CHARACTER*(*) STRING,TMP
      INTEGER SIGLEN
      INTEGER L
      SAVE
      L = LEN (TMP)
      IF (L.EQ.0) CALL FATALERR ('ADDSTF','internal error')
      IF (SIGLEN+L.GT.LEN (STRING)) CALL FATALERR
     &('ADDSTF','internal error')
      STRING(SIGLEN+1:SIGLEN+L) = TMP
      SIGLEN = SIGLEN+L
      RETURN
      END
