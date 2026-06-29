** $Id: wrsint.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE WRSINT (XNAME,X)
      IMPLICIT NONE
      CHARACTER*(*) XNAME
      INTEGER X
      LOGICAL INIT
      INTEGER UNIT
      CHARACTER*80 SPACE
      CHARACTER*1 Q
      COMMON /WR_SYS_1/ INIT,UNIT
      COMMON /WR_SYS_2/ SPACE,Q
      SAVE
      IF (.NOT.INIT) CALL FATALERR ('WRSINT','system not initialized')
      WRITE (UNIT,'(1X,2A,I11)') XNAME,' = ',X
      RETURN
      END
