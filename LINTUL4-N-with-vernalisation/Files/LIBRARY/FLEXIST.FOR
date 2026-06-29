** $Id: flexist.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      LOGICAL FUNCTION FLEXIST (FILE_NAME)
      IMPLICIT NONE
      CHARACTER*(*) FILE_NAME
      CHARACTER*132 FILE_NAME_L
      LOGICAL THERE
      INTEGER IL, ILEN
      SAVE
      FILE_NAME_L = FILE_NAME
      CALL FLNAME (FILE_NAME_L)
      IL = ILEN (FILE_NAME_L)
      INQUIRE (FILE=FILE_NAME_L(1:IL),EXIST=THERE)
      FLEXIST = THERE
      RETURN
      END
