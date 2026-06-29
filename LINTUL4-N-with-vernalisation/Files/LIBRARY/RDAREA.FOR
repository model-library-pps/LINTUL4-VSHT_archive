* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDAREA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
      INTEGER ILDEC,IFND
      REAL X
      DIMENSION X(ILDEC)
      CHARACTER*(*) XNAME
      DOUBLE PRECISION D(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IS
      SAVE
      CALL RDDATA (6,'RDAREA',0,0,' ',IS,XNAME,'R',
     $D,X,I,C,L,1,ILDEC,1,1,1,IFND)
      RETURN
      END
