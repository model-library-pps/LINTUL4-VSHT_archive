** $Id: getchr.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE GETCHR (IUNIT,INCHAR,CHARS,EOF)
      IMPLICIT NONE
      INTEGER IUNIT,INCHAR
      CHARACTER*(*) CHARS
      LOGICAL EOF
      INTEGER I,IOS,IUNOLD,IC,ILR,ILEN
      CHARACTER BUFFER*140
      LOGICAL INIT
      SAVE
      DATA INIT/.FALSE./, IUNOLD/-1/
      EOF = .FALSE.
      IF (INCHAR.GT.140)
     $CALL FATALERR ('GETCHR','too many characters requested')
      IF (.NOT.INIT .OR. IUNIT.NE.IUNOLD) THEN
      BUFFER = ' '
      READ (IUNIT,'(A)',IOSTAT=IOS) BUFFER
      EOF = IOS.LT.0
      IC  = 0
      ILR = ILEN (BUFFER)
      IUNOLD = IUNIT
      END IF
      I = 0
      CHARS = ' '
10    IF (I.LT.INCHAR .AND. .NOT.EOF) THEN
      I = I + 1
20    IF (IC.EQ.ILR .AND..NOT.EOF) THEN
      BUFFER = ' '
      READ (IUNIT,'(A)',IOSTAT=IOS) BUFFER
      EOF = IOS.LT.0
      IC  = 0
      ILR = ILEN (BUFFER)
      GOTO 20
      END IF
      IC = IC + 1
      IF (.NOT.EOF) THEN
      CHARS(I:I) = BUFFER(IC:IC)
      END IF
      GOTO 10
      END IF
      INIT = .NOT.EOF
      RETURN
      END
