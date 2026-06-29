** $Id: rdslog.for 1.1 1997/10/02 16:08:13 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDSLOG (XNAME,X)
      IMPLICIT NONE
      CHARACTER*(*) XNAME
      LOGICAL X
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IL,IS
      SAVE
      IL = 0
      CALL RDDATA (5,'RDSLOG',0,0,' ',IS,XNAME,'L',D,R,I,C,L,
     $1,1,1,1,1,IL)
      X = L(1)
      RETURN
      END
