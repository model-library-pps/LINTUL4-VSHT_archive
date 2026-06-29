* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDAINT (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
      INTEGER ILDEC,IFND,X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
      DOUBLE PRECISION D(1)
      REAL             R(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IS
      SAVE
      CALL RDDATA (6,'RDAINT',0,0,' ',IS,XNAME,'I',
     $D,R,X,C,L,1,1,ILDEC,1,1,IFND)
      RETURN
      END
