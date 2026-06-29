** $Id: rdmint.for 1.1 1997/10/02 16:08:13 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDMINT (X)
      IMPLICIT NONE
      INTEGER X
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IL,IS
      SAVE
      I(1) = X
      CALL RDDATA (10,'RDMINT',0,0,' ',IS,' ','I',D,R,I,C,L,
     $1,1,1,1,1,IL)
      RETURN
      END
