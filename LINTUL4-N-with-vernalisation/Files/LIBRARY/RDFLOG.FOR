** $Id: rdflog.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDFLOG (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
      INTEGER ILDEC,IVALS
      LOGICAL X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      INTEGER IS
      SAVE
      CALL RDDATA (7,'RDFLOG',0,0,' ',IS,XNAME,'L',
     $D,R,I,C,X,1,1,1,1,ILDEC,IVALS)
      RETURN
      END
