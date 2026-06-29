* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDSDOU (XNAME,X)
      IMPLICIT NONE
      CHARACTER*(*) XNAME
      DOUBLE PRECISION X
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IL,IS
      SAVE
      IL = 0
      CALL RDDATA (5,'RDSDOU',0,0,' ',IS,XNAME,'D',D,R,I,C,L,
     $1,1,1,1,1,IL)
      X = D(1)
      RETURN
      END
