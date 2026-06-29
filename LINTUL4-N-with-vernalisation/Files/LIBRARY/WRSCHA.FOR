** $Id: wrscha.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE WRSCHA (XNAME,X)
      IMPLICIT NONE
      CHARACTER*(*) XNAME,X
      LOGICAL INIT
      INTEGER UNIT
      CHARACTER*80 SPACE
      CHARACTER*1 Q
      COMMON /WR_SYS_1/ INIT,UNIT
      COMMON /WR_SYS_2/ SPACE,Q
      INTEGER X_L,ILEN
      SAVE
      IF (.NOT.INIT) CALL FATALERR ('WRSCHA','system not initialized')
      X_L = MAX (1,ILEN (X))
      WRITE (UNIT,'(1X,5A)') XNAME,' = ',Q,X(1:X_L),Q
      RETURN
      END
