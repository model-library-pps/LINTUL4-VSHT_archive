* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDFREA (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
      INTEGER ILDEC,IVALS
      REAL X
      DIMENSION X(ILDEC)
      CHARACTER*(*) XNAME
      DOUBLE PRECISION D(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IS
      SAVE
      CALL RDDATA (7,'RDFREA',0,0,' ',IS,XNAME,'R',
     $D,X,I,C,L,1,ILDEC,1,1,1,IVALS)
      RETURN
      END
