** $Id: wralog.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE WRALOG (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
      INTEGER ILDEC,IFND
      CHARACTER*(*) XNAME
      LOGICAL X(ILDEC)
      LOGICAL INIT
      INTEGER UNIT
      CHARACTER*80 SPACE
      CHARACTER*1 Q
      COMMON /WR_SYS_1/ INIT,UNIT
      COMMON /WR_SYS_2/ SPACE,Q
      INTEGER XNAME_L, ILEN, I1
      SAVE
      IF (.NOT.INIT) CALL FATALERR ('WRALOG','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRALOG','error')
      XNAME_L = ILEN (XNAME)
      IF (X(1)) THEN
      WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = .TRUE.'
      ELSE
      WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = .FALSE.'
      END IF
      IF (IFND.GT.1) THEN
      DO I1=2,IFND
      IF (X(I1)) THEN
      WRITE (UNIT,'(1X,2A)')  SPACE(1:XNAME_L),'   .TRUE.'
      ELSE
      WRITE (UNIT,'(1X,2A)')  SPACE(1:XNAME_L),'   .FALSE.'
      END IF
      END DO
      END IF
      RETURN
      END
