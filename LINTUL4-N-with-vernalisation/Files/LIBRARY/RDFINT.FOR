* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDFINT (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
      INTEGER ILDEC,IVALS,X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
      DOUBLE PRECISION D(1)
      REAL             R(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IS
      SAVE
      CALL RDDATA (7,'RDFINT',0,0,' ',IS,XNAME,'I',
     $D,R,X,C,L,1,1,ILDEC,1,1,IVALS)
      RETURN
      END
