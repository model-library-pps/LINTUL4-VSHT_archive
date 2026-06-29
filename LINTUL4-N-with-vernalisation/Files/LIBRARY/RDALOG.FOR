** $Id: rdalog.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDALOG (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
      INTEGER ILDEC,IFND
      LOGICAL X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      INTEGER IS
      SAVE
      CALL RDDATA (6,'RDALOG',0,0,' ',IS,XNAME,'L',
     $D,R,I,C,X,1,1,1,1,ILDEC,IFND)
      RETURN
      END
