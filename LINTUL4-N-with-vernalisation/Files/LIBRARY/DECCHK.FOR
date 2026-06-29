* This file was intentionally removed of
* comment lines and indentation
      LOGICAL FUNCTION DECCHK (STRING)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER I,IN,IS,ILEN
      INTEGER ITYP(0:14), JUMPTB(0:9,0:5)
      CHARACTER NUM1*14
      SAVE
      DATA   NUM1 /'+-1234567890.E'/
      DATA   ITYP /0,1,1,3,3,3,3,3,3,3,3,3,3,2,4/
      DATA JUMPTB /0,0,0,0,0,0,0,0,0,0, 0,2,0,0,0,0,7,0,0,0,
     $0,3,3,0,5,0,0,0,0,0, 0,4,4,5,4,5,8,8,9,0,
     $0,0,0,0,6,6,0,0,0,0, 0,0,0,0,4,5,0,0,8,9/
      ILEN = LEN (STRING)
      I    = 0
      IS   = 1
10    IF (IS.GT.0 .AND. I.LT.ILEN) THEN
      I = I + 1
      IN = INDEX (NUM1,STRING(I:I))
      IS = JUMPTB (IS,ITYP(IN))
      GOTO 10
      END IF
      IS = JUMPTB (IS,5)
      DECCHK = IS.GT.0
      RETURN
      END
