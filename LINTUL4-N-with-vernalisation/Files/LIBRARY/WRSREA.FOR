** $Id: wrsrea.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE WRSREA (XNAME,X)
      IMPLICIT NONE
      CHARACTER*(*) XNAME
      REAL X
      LOGICAL INIT
      INTEGER UNIT
      CHARACTER*80 SPACE
      CHARACTER*1 Q
      COMMON /WR_SYS_1/ INIT,UNIT
      COMMON /WR_SYS_2/ SPACE,Q
      SAVE
      IF (.NOT.INIT) CALL FATALERR ('WRSREA','system not initialized')
      WRITE (UNIT,'(1X,2A,1P,G13.6)') XNAME,' = ',X
      RETURN
      END
