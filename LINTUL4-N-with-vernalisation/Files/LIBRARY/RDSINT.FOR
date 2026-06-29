* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDSINT (XNAME,X)
      IMPLICIT NONE
      INTEGER X
      CHARACTER*(*) XNAME
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IL,IS
      SAVE
      IL = 0
      CALL RDDATA (5,'RDSINT',0,0,' ',IS,XNAME,'I',D,R,I,C,L,
     $1,1,1,1,1,IL)
      X = I(1)
      RETURN
      END
