** $Id: flname.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE FLNAME (STRING)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      SAVE
      CALL LOWERC (STRING)
      RETURN
      END
