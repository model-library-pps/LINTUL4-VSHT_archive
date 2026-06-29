* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDACHA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
      INTEGER ILDEC,IFND
      CHARACTER*(*) XNAME,X
      DIMENSION X(ILDEC)
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      LOGICAL          L(1)
      INTEGER IS
      SAVE
      CALL RDDATA (6,'RDACHA',0,0,' ',IS,XNAME,'C',
     $D,R,I,X,L,1,1,1,ILDEC,1,IFND)
      RETURN
      END
