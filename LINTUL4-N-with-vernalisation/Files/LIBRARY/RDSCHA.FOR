* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDSCHA (XNAME,X)
      IMPLICIT NONE
      CHARACTER*(*) XNAME,X
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      LOGICAL          L(1)
      INTEGER IL,IS
      SAVE
      IL = 0
      CALL RDDATA (5,'RDSCHA',0,0,' ',IS,XNAME,'C',D,R,I,X,L,
     $1,1,1,1,1,IL)
      RETURN
      END
