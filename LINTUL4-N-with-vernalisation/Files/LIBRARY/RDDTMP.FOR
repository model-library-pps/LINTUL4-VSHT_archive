* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDDTMP (IUNIT)
      IMPLICIT NONE
      INTEGER IUNIT
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IL,IS
      SAVE
      CALL RDDATA (9,'RDDTMP',IUNIT,0,C,IS,C,C,D,R,I,C,L,1,1,1,1,1,IL)
      RETURN
      END
