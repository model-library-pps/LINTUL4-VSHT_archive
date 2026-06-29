** $Id: wrinit.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      BLOCK DATA WRINIT_DATA
      IMPLICIT NONE
      LOGICAL INIT
      INTEGER UNIT
      CHARACTER*80 SPACE
      CHARACTER*1 Q
      COMMON /WR_SYS_1/ INIT,UNIT
      COMMON /WR_SYS_2/ SPACE,Q
      SAVE
      DATA INIT /.FALSE./
      DATA SPACE /' '/
      END
      SUBROUTINE WRINIT (UNIT_X,FILE)
      IMPLICIT NONE
      INTEGER UNIT_X
      CHARACTER*(*) FILE
      LOGICAL INIT
      INTEGER UNIT
      CHARACTER*80 SPACE
      CHARACTER*1 Q
      COMMON /WR_SYS_1/ INIT,UNIT
      COMMON /WR_SYS_2/ SPACE,Q
      LOGICAL OPEN
      INTEGER IL,ILEN
      SAVE
      Q    = CHAR (39)
      UNIT = UNIT_X
      IL   = ILEN (FILE)
      IF (IL.EQ.0) CALL FATALERR ('WRINIT','empty file name')
      INQUIRE (UNIT=UNIT,OPENED=OPEN)
      IF (.NOT.OPEN) CALL FOPENS (UNIT,FILE,'NEW','DEL')
      INIT = .TRUE.
      RETURN
      END
