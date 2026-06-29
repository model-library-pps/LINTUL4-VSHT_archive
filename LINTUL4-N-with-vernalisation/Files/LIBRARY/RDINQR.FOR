* This file was intentionally removed of
* comment lines and indentation
      LOGICAL FUNCTION RDINQR (XNAME)
      IMPLICIT NONE
      CHARACTER*(*) XNAME
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IFLAG,IL
      SAVE
      CALL RDDATA (8,'RDINQR',0,0,C,IFLAG,XNAME,C,D,R,I,C,L,
     $1,1,1,1,1,IL)
      RDINQR = IFLAG.GT.0
      RETURN
      END
