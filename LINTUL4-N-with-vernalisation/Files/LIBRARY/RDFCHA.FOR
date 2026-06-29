* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDFCHA (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
      INTEGER ILDEC,IVALS
      CHARACTER*(*) XNAME,X
      DIMENSION X(ILDEC)
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      LOGICAL          L(1)
      INTEGER IS
      SAVE
      CALL RDDATA (7,'RDFCHA',0,0,' ',IS,XNAME,'C',
     $D,R,I,X,L,1,1,1,ILDEC,1,IVALS)
      RETURN
      END
