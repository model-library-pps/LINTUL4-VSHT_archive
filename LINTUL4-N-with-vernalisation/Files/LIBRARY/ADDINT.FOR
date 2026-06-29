** $Id: addint.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE ADDINT (STRING,SIGLEN,I)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER SIGLEN,I
      CHARACTER*11 TMP
      INTEGER ISTART,ILEN,IS,IL,L
      SAVE
      WRITE (TMP,'(I11)') I
      IS = ISTART (TMP)
      IL = ILEN (TMP)
      L  = IL-IS+1
      IF (SIGLEN+L.GT.LEN (STRING)) CALL FATALERR
     &('ADDINT','internal error')
      STRING(SIGLEN+1:SIGLEN+L) = TMP(IS:IL)
      SIGLEN = SIGLEN+L
      RETURN
      END
