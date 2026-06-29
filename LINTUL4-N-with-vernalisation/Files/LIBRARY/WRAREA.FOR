** $Id: wrarea.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE WRAREA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
      CHARACTER*(*) XNAME
      INTEGER ILDEC,IFND
      REAL X(ILDEC)
      LOGICAL INIT
      INTEGER UNIT
      CHARACTER*80 SPACE
      CHARACTER*1 Q
      COMMON /WR_SYS_1/ INIT,UNIT
      COMMON /WR_SYS_2/ SPACE,Q
      INTEGER XNAME_L, ILEN, I1, I2
      INTEGER NCOL
      PARAMETER (NCOL=3)
      SAVE
      IF (.NOT.INIT) CALL FATALERR ('WRAREA','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRAREA','error')
      XNAME_L = ILEN (XNAME)
      WRITE (UNIT,'(1X,2A,1P,10G13.6)')
     &XNAME(1:XNAME_L),' = ',(X(I1),I1=1,MIN (NCOL, IFND))
      IF (IFND.GT.NCOL) THEN
      DO I1=NCOL+1,IFND,NCOL
      WRITE (UNIT,'(1X,2A,1P,10G13.6)')
     &SPACE(1:XNAME_L),'   ',
     &(X(I2),I2=I1,MIN (NCOL+I1-1, IFND))
      END DO
      END IF
      RETURN
      END
