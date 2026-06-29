* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDFROM (IS,FATAL)
      IMPLICIT NONE
      INTEGER IS
      LOGICAL FATAL
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IFATAL
      SAVE
      IF (FATAL) THEN
      IFATAL = 1
      ELSE
      IFATAL = 0
      END IF
      CALL RDDATA (2,'RDFROM',0,0,' ',IS,' ',' ',D,R,I,C,L,
     $1,1,1,1,1,IFATAL)
      RETURN
      END
