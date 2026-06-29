** $Id: wracha.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE WRACHA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
      INTEGER ILDEC,IFND
      CHARACTER*(*) XNAME,X(ILDEC)
      LOGICAL INIT
      INTEGER UNIT
      CHARACTER*80 SPACE
      CHARACTER*1 Q
      COMMON /WR_SYS_1/ INIT,UNIT
      COMMON /WR_SYS_2/ SPACE,Q
      INTEGER XNAME_L, ILEN, I1, I2
      INTEGER NCOL
      PARAMETER (NCOL=1)
      SAVE
      IF (.NOT.INIT) CALL FATALERR ('WRACHA','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRACHA','error')
      XNAME_L = ILEN (XNAME)
      WRITE (UNIT,'(1X,2A,30A)')
     &XNAME(1:XNAME_L),' = ',(Q,X(I1),Q,I1=1,MIN (NCOL, IFND))
      IF (IFND.GT.NCOL) THEN
      DO I1=NCOL+1,IFND,NCOL
      WRITE (UNIT,'(1X,2A,30A)')
     &SPACE(1:XNAME_L),'   ',
     &(Q,X(I2),Q,I2=I1,MIN (NCOL+I1-1, IFND))
      END DO
      END IF
      RETURN
      END
