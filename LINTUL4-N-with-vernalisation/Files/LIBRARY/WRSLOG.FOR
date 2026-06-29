** $Id: wrslog.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE WRSLOG (XNAME,X)
      IMPLICIT NONE
      CHARACTER*(*) XNAME
      LOGICAL X
      LOGICAL INIT
      INTEGER UNIT
      CHARACTER*80 SPACE
      CHARACTER*1 Q
      COMMON /WR_SYS_1/ INIT,UNIT
      COMMON /WR_SYS_2/ SPACE,Q
      SAVE
      IF (.NOT.INIT) CALL FATALERR ('WRSLOG','system not initialized')
      IF (X) THEN
      WRITE (UNIT,'(1X,2A)') XNAME,' = .TRUE.'
      ELSE
      WRITE (UNIT,'(1X,2A)') XNAME,' = .FALSE.'
      END IF
      RETURN
      END
