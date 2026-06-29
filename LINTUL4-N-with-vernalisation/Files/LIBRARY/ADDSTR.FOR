** $Id: addstr.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE ADDSTR (STRING,SIGLEN,TMP)
      IMPLICIT NONE
      CHARACTER*(*) STRING,TMP
      INTEGER SIGLEN
      INTEGER ISTART,ILEN,IS,IL,L
      SAVE
      IS = ISTART (TMP)
      IL = ILEN (TMP)
      IF (IL.EQ.0) CALL FATALERR ('ADDSTR','internal error')
      L  = IL-IS+1
      IF (SIGLEN+L.GT.LEN (STRING)) CALL FATALERR
     &('ADDSTR','internal error')
      STRING(SIGLEN+1:SIGLEN+L) = TMP(IS:IL)
      SIGLEN = SIGLEN+L
      RETURN
      END
