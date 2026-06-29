* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDINIT (IUNIT,IULOG,DATFIL)
      IMPLICIT NONE
      INTEGER IUNIT,IULOG
      CHARACTER*(*) DATFIL
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IL,IS
      SAVE
      IL = 0
      CALL RDDATA (3,'RDINIT',IUNIT,IULOG,DATFIL,IS,' ',' ',
     $D,R,I,C,L,1,1,1,1,1,IL)
      RETURN
      END
