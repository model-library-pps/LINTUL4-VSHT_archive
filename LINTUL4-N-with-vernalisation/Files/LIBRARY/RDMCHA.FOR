** $Id: rdmcha.for 1.1 1997/10/02 16:08:13 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDMCHA (X)
      IMPLICIT NONE
      CHARACTER*(*) X
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      LOGICAL          L(1)
      INTEGER IL,IS
      SAVE
      CALL RDDATA (10,'RDMCHA',0,0,' ',IS,' ','C',D,R,I,X,L,
     $1,1,1,1,1,IL)
      RETURN
      END
