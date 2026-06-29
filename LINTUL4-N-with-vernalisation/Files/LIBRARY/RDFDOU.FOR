* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDFDOU (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
      INTEGER ILDEC,IVALS
      DOUBLE PRECISION X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IS
      SAVE
      CALL RDDATA (7,'RDFDOU',0,0,' ',IS,XNAME,'D',
     $X,R,I,C,L,ILDEC,1,1,1,1,IVALS)
      RETURN
      END
