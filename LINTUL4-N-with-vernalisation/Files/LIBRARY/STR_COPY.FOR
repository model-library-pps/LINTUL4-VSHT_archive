** $Id: str_copy.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE STR_COPY (SOURCE_S,TARGET_S,OK)
      IMPLICIT NONE
      CHARACTER*(*) SOURCE_S,TARGET_S
      LOGICAL OK
      INTEGER ILS,ILT,ILEN
      SAVE
      OK  = .TRUE.
      ILS = MAX (1,ILEN (SOURCE_S))
      ILT = LEN (TARGET_S)
      IF (ILS.GT.ILT) THEN
      OK  = .FALSE.
      ILS = ILT
      END IF
      TARGET_S = SOURCE_S(1:ILS)
      RETURN
      END
