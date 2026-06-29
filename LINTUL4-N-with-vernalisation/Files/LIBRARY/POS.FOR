* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE POS (IX,IY,TEXT)
      IMPLICIT NONE
      INTEGER IX,IY
      CHARACTER*(*) TEXT
      INTEGER IXHS,IYHS,ISTART
      CHARACTER*2 XH, YH
      SAVE
      WRITE (XH,'(I2)') IX
      WRITE (YH,'(I2)') IY
      IXHS = ISTART (XH)
      IYHS = ISTART (YH)
      WRITE (*,'(1X,7A,$)')
     $CHAR(27),'[',YH(IYHS:2),';',XH(IXHS:2),'H',TEXT
      RETURN
      END
